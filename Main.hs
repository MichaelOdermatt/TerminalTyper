module Main (main) where
import Words (wordBank)
import Data.Char
import System.IO
import System.Random

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    mainLoop

mainLoop :: IO ()
mainLoop = do
    randomNums <- getListOfRandomInts 50 (length wordBank)
    let initialWordList = getWordsFromWordBank randomNums
    mainLoop' initialWordList 0

mainLoop' :: [String] -> Int -> IO ()
mainLoop' wordList wordIndex = do
    randomNum <- getRandomInt (length wordBank - 1)
    let extendedWordList = wordList ++ [wordBank !! randomNum]
    let currentWord = wordList !! wordIndex
    putStr clearScreen
    putStrLn ""
    putStrLn (insertLineBreaks (joinStringsWithSpaces (highlightStringInList extendedWordList currentWord)))
    putStrLn ""
    wordFromUser <- getUserInput
    -- let wasSuccessful = (wordFromList == wordFromUser) will use this later
    mainLoop' extendedWordList (wordIndex + 1)

getUserInput :: IO [Char]
getUserInput = getUserInput' ""

getUserInput' :: String -> IO [Char]
getUserInput' xs = do
    inputChar <- getChar
    case inputChar of
        c | isSpace c ->
            return xs
        '\DEL' ->
            if xs == "" then
                getUserInput' ""
            else do
                putStr removeLastCharacter
                getUserInput' (init xs)
        _ -> do
            putChar inputChar
            getUserInput' (xs ++ [inputChar])

getRandomInt :: Int -> IO Int
getRandomInt upperRange = do randomRIO (0, upperRange) :: IO Int

-- | First argument is the length of the list. The second argument is the upper range for the list values (0 - upperRange)
getListOfRandomInts :: Int -> Int -> IO [Int]
getListOfRandomInts 0 _ = return []
getListOfRandomInts x upperRange = do
    num <- getRandomInt upperRange
    nums <- getListOfRandomInts (x - 1) upperRange
    return (num:nums)

-------------------- Pure Functions

-- | Takes a List of indexes and returns the Strings at thoes indexes in the wordBank
getWordsFromWordBank :: [Int] -> [String]
getWordsFromWordBank = map (wordBank !!)

-- | Returns a the given list with the first occurance of the given word highlighted
-- TODO fix this since this wont work if the word shows up more than once
highlightStringInList :: [String] -> String -> [String]
highlightStringInList [] _ = []
highlightStringInList (word:words) wordToHighlight
    | word == wordToHighlight = (textColorCyan ++ word ++ textColorReset) : words
    | otherwise = word : highlightStringInList words wordToHighlight

{-  |
    Takes a string and inserts a line break at the next space character after every x 
    amount of characters (x is the return of the lineCharacterLimit function).
-}
insertLineBreaks :: String -> String
insertLineBreaks x = insertLineBreaks' x 1

insertLineBreaks' :: String -> Int -> String
insertLineBreaks' [] _ = []
insertLineBreaks' (x:xs) count
    | count >= lineCharacterLimit && x == ' ' = '\n' : insertLineBreaks' xs 1
    | otherwise = x : insertLineBreaks' xs (count + 1)

joinStringsWithSpaces :: [String] -> String
joinStringsWithSpaces = foldr (\ x y -> x ++ " " ++ y) ""

removeLastCharacter :: String
removeLastCharacter = "\b \b"

lineCharacterLimit :: Int
lineCharacterLimit = 50

-------------------- ANSI escape sequences
-- all excape codes can be found here https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
textColorReset :: String
textColorReset = "\ESC[0m"

textColorCyan :: String
textColorCyan = "\ESC[36m"

clearScreen :: String
clearScreen = "\ESC[2J"