import Words (wordBank)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.List (intercalate)
import System.IO (BufferMode (LineBuffering), BufferMode (NoBuffering), hSetBuffering, stdin, hSetEcho)
import System.Random (randomRIO)
import GHC.IO.Handle (hWaitForInput)

-------------------- IO Functions

main :: IO ()
main = do
    typingDuration <- promptUserForTypingDuration
    case typingDuration of
        Just duration -> startTypingLoop duration
        Nothing -> do
            putStrLn ""
            putStrLn "Error: An invalid input was given."
            return ()

promptUserForTypingDuration :: IO (Maybe Int)
promptUserForTypingDuration = do
    hSetBuffering stdin LineBuffering
    hSetEcho stdin True
    putStr clearScreen
    putStrLn ""
    putStrLn "Please enter the duration you would like to type for."
    putStrLn ""
    readMaybe <$> getLine

startTypingLoop :: Int -> IO ()
startTypingLoop typingDuration = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    startTime <- getCurrentTime
    typingLoop (addUTCTime (convertToNominalDiffTime typingDuration) startTime) typingDuration

{-
    | The main loop function which takes a UTCTime deadline value.
    This value represents the time limit in which the user can type.
-}
typingLoop :: UTCTime -> Int -> IO ()
typingLoop deadline typingDuration = do
    randomNums <- getListOfRandomInts 100 (length wordBank - 1)
    let initialWordList = getWordsFromWordBank randomNums
    typingLoop' initialWordList 0 0 typingDuration deadline

typingLoop' :: [String] -> Int -> Int -> Int -> UTCTime -> IO ()
typingLoop' wordList wordIndex numOfCorrectWords typingDuration deadline = do
    let allStringGroups = breakStringsIntoGroups $ highlightStringInList wordList wordIndex
    let desiredGroups = getDesiredStringGroups allStringGroups wordIndex
    putStr clearScreen
    putStrLn ""
    putStrLn (intercalate " " (intercalate ["\n"] desiredGroups))
    putStrLn ""
    possibleWordFromUser <- getUserInputWithTimer deadline
    case possibleWordFromUser of
        Just wordFromUser -> do
            let wordFromList = wordList !! wordIndex
            if wordFromList == wordFromUser then
                typingLoop' wordList (wordIndex + 1) (numOfCorrectWords + 1) typingDuration deadline
            else
                typingLoop' wordList (wordIndex + 1) numOfCorrectWords typingDuration deadline
        Nothing -> do
            putStr clearScreen
            putStrLn ""
            putStrLn ("Your typing speed is " ++ show (calcWordsPerMinute numOfCorrectWords typingDuration) ++ " wpm")

-------------------- Getting inputs from the user character by character

{-
    | This function gets the input word from the user. This function has a deadline
    parameter which is the amount of time the user has to input the next word before
    the function times out.
-}
getUserInputWithTimer :: UTCTime -> IO (Maybe String)
getUserInputWithTimer = getUserInputWithTimer' ""

getUserInputWithTimer' :: String -> UTCTime -> IO (Maybe String)
getUserInputWithTimer' xs deadline = do
    now <- getCurrentTime
    let remainingTime = round (diffUTCTime deadline now * 1000)
    inputReady <- hWaitForInput stdin remainingTime
    if inputReady then do
        inputChar <- getChar
        case inputChar of
            c | isSpace c -> return (Just xs)
            '\DEL' ->
                if null xs then
                    getUserInputWithTimer' "" deadline
                else do
                    putStr removeLastCharacter
                    getUserInputWithTimer' (init xs) deadline
            _ -> do
                putChar inputChar
                getUserInputWithTimer' (xs ++ [inputChar]) deadline
    else return Nothing

-------------------- For generating random numbers

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

-- | Takes a list of indexes and returns the Strings at thoes indexes in the wordBank
getWordsFromWordBank :: [Int] -> [String]
getWordsFromWordBank = map (wordBank !!)

-- | Returns a the given list with the word at the given index highlighted using the textColorCyan function
highlightStringInList :: [String] -> Int -> [String]
highlightStringInList words wordIndex = highlightStringInList' words wordIndex 0

highlightStringInList' :: [String] -> Int -> Int-> [String]
highlightStringInList' [] _ _ = []
highlightStringInList' (word:words) wordIndex count
    | wordIndex == count = (textColorCyan ++ word ++ textColorReset) : words
    | otherwise = word : highlightStringInList' words wordIndex (count + 1)

{-
    | Takes a list of strings and splits them into multiple lists, each list's length is determined by
    the line character limit function.
-}
breakStringsIntoGroups :: [String] -> [[String]]
breakStringsIntoGroups [] = []
breakStringsIntoGroups (word:words) = breakStringsIntoGroups' words [word] 0

breakStringsIntoGroups' :: [String] -> [String] -> Int -> [[String]]
breakStringsIntoGroups' [] currentSubList _ = [currentSubList]
breakStringsIntoGroups' (word:words) currentSubList count
    | count + wordLength >= lineCharacterLimit = currentSubList : breakStringsIntoGroups' words [word] wordLength
    | otherwise = breakStringsIntoGroups' words (currentSubList ++ [word]) (count + wordLength)
    where
        wordLength = length (removeEscapeSequenceFromString word)

{-
    | Takes a list of lists of strings and a word index. Returns both the list that the corresponding word to
    the given word index is contained in, and the following list.
-}
getDesiredStringGroups :: [[String]] -> Int -> [[String]]
getDesiredStringGroups groups wordIndex = getDesiredStringGroups' groups wordIndex 0

getDesiredStringGroups' :: [[String]] -> Int -> Int -> [[String]]
getDesiredStringGroups' [] _ _ = []
getDesiredStringGroups' (group:groups) wordIndex count
    | count + length group <= wordIndex = getDesiredStringGroups' groups wordIndex (count + length group)
    | otherwise = group : [nextGroup]
    where
        nextGroup
            | null groups = []
            | otherwise = head groups


-- | Returns the given string will all Escape sequences (eg. \ESC***m) removed
removeEscapeSequenceFromString :: String -> String
removeEscapeSequenceFromString [] = []
removeEscapeSequenceFromString (x:xs)
    | x == '\ESC' = removeEscapeSequenceFromString (getAllElementsAfterPoint xs 'm')
    | otherwise = x : removeEscapeSequenceFromString xs

-- | Returns the entire list up until and including the given value
getAllElementsUpToPoint :: Eq a => [a] -> a -> [a]
getAllElementsUpToPoint xs val = takeWhile (/= val) xs ++ [val]

-- | Returns the remainder of the list after the given value 
getAllElementsAfterPoint :: Eq a => [a] -> a -> [a]
getAllElementsAfterPoint xs val = tail (dropWhile (/= val) xs)

calcWordsPerMinute :: Int -> Int -> Int
calcWordsPerMinute numOfWords typingDuration = floor ((numOfWords `divideIntToFloat` typingDuration) * 60.0)

divideIntToFloat :: Int -> Int -> Float
divideIntToFloat x y = fromIntegral x / fromIntegral y

convertToNominalDiffTime :: Int -> NominalDiffTime
convertToNominalDiffTime seconds = realToFrac (toRational seconds) :: NominalDiffTime

removeLastCharacter :: String
removeLastCharacter = "\b \b"

-- | The character limit per line when printing the word list.
lineCharacterLimit :: Int
lineCharacterLimit = 80

-------------------- ANSI escape sequences
-- all excape codes can be found here https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
textColorReset :: String
textColorReset = "\ESC[0m"

textColorCyan :: String
textColorCyan = "\ESC[36m"

clearScreen :: String
clearScreen = "\ESC[2J"