module Main (main) where
import Words (wordList)
import Data.Char
import System.IO
import System.Random

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering   
    mainLoop 0

mainLoop :: Int -> IO ()
mainLoop numOfCorrectWords = do
    num <- randomRIO (0, length wordList) :: IO Int
    let wordFromList = wordList !! num
    print ("Correct words: " ++ show numOfCorrectWords)
    print wordFromList
    wordFromUser <- getUserInput ""
    if wordFromList == wordFromUser then 
        mainLoop (numOfCorrectWords + 1)
    else
        mainLoop numOfCorrectWords
    
getUserInput :: String -> IO [Char]
getUserInput s = do
    inputChar <- getChar
    if isSpace inputChar then
        return s
    else 
        getUserInput (s ++ [inputChar])