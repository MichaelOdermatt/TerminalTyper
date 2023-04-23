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
mainLoop = mainLoop' 0

mainLoop' :: Int -> IO ()
mainLoop' numOfCorrectWords = do
    randomInt <- getRandomInt (length wordBank)
    let wordFromList = wordBank !! randomInt

    putStr clearScreen
    print ("Correct words: " ++ show numOfCorrectWords)
    print wordFromList

    wordFromUser <- getUserInput
    if wordFromList == wordFromUser then
        mainLoop' (numOfCorrectWords + 1)
    else
        mainLoop' numOfCorrectWords

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

-------------------- Pure Functions

-- | Takes an array of indexes and returns an array of the Strings at thoes indexes in the wordBank
getWordsFromWordBank :: [Int] -> [String]
getWordsFromWordBank = map (wordBank !!)

-------------------- ANSI escape sequences
clearScreen :: String
clearScreen = "\ESC[2J"

removeLastCharacter :: String
removeLastCharacter = "\b \b"