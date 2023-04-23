module Main (main) where
import Words (wordList)
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
    num <- randomRIO (0, length wordList) :: IO Int
    let wordFromList = wordList !! num
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
                putStr "\b \b"
                getUserInput' (init xs)
        _ -> do  
            putChar inputChar
            getUserInput' (xs ++ [inputChar])