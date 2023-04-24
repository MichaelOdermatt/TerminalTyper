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
    mainLoop' initialWordList

mainLoop' :: [String] -> IO ()
mainLoop' wordList = do
    randomNum <- getRandomInt (length wordBank)
    let extendedWordList = wordList ++ [wordBank !! randomNum]
    let firstWordFromList = head extendedWordList
    printListOfWords extendedWordList
    putStrLn ""
    wordFromUser <- getUserInput
    -- let wasSuccessful = (wordFromList == wordFromUser) will use this later
    mainLoop' (tail extendedWordList)

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
printListOfWords :: [String] -> IO ()
printListOfWords (word:words) = do
    putStr clearScreen
    putStrLn ""
    putStr textColorCyan
    putStr word
    putStr textColorReset
    printListOfWords' words

printListOfWords' :: [String] -> IO ()
printListOfWords' (word:words) 
    | null words = do 
        return ()
    | otherwise = do
        putStr (" " ++ word)
        printListOfWords' words

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