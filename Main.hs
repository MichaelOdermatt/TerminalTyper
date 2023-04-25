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
    randomNums <- getListOfRandomInts 10 (length wordBank)
    let initialWordList = getWordsFromWordBank randomNums
    mainLoop' initialWordList 0

mainLoop' :: [String] -> Int -> IO ()
mainLoop' wordList wordIndex = do
    randomNum <- getRandomInt (length wordBank - 1)
    let extendedWordList = wordList ++ [wordBank !! randomNum]
    let currentWord = wordList !! wordIndex
    printListOfWords extendedWordList wordIndex
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

-- | Prints the list of words to the user, with the first word in the list colored differently
-- TODO: Two issues with this method 1: the output doesn't wrap at spaces which means words get cut off
printListOfWords :: [String] -> Int -> IO ()
printListOfWords words highlightedWordIndex = do
    putStr clearScreen
    putStrLn ""
    printListOfWords' words 0 highlightedWordIndex

printListOfWords' :: [String] -> Int -> Int -> IO ()
printListOfWords' (word:words) currentIndex highlightedWordIndex 
    | null words = do 
        return ()
    | currentIndex == highlightedWordIndex = do
        putStr textColorCyan
        putStr (" " ++ word)
        putStr textColorReset
        printListOfWords' words (currentIndex + 1) highlightedWordIndex
    | otherwise = do
        putStr (" " ++ word)
        printListOfWords' words (currentIndex + 1) highlightedWordIndex

-------------------- Pure Functions

-- | Takes a List of indexes and returns the Strings at thoes indexes in the wordBank
getWordsFromWordBank :: [Int] -> [String]
getWordsFromWordBank = map (wordBank !!)

-------------------- ANSI escape sequences
-- all excape codes can be found here https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
textColorReset :: String
textColorReset = "\ESC[0m"

textColorCyan :: String
textColorCyan = "\ESC[36m"

clearScreen :: String
clearScreen = "\ESC[2J"

removeLastCharacter :: String
removeLastCharacter = "\b \b"