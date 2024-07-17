-- Assessment 2

module Main where 

import Base
import Data.Char (toLower)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian, DayOfMonth)
import Control.Monad (when)
import System.Exit (exitSuccess)

-- EXTRA USEFUL FUNCTIONS ARE DEFINED HERE

-- Function for returning only the statuses of guesses returned via the 'checkGuess' function 
getGuessStatuses :: [(Char, Status)] -> [Status]
getGuessStatuses [] = []
getGuessStatuses (x : xs) = snd x : getGuessStatuses xs

-- Function for checking if the user has won 
checkWin :: [Status] -> Bool
checkWin [Here, Here, Here, Here, Here] = True 
checkWin _ = False

-- Function for getting the characters that are nowhere in a word
nowhereChars :: [(Char, Status)] -> String 
nowhereChars [] = []
nowhereChars (x : xs) 
    | snd x == Nowhere = [fst x] ++ nowhereChars xs
    | otherwise = nowhereChars xs

-- Function for removing a list of Chars from a string. First input is substring to delete and second is string to delete from.
removeSubstring :: String -> String -> String
removeSubstring str = do
    let doesNotOccur = (`notElem` str)
    filter doesNotOccur

-- Function that outputs the alphabet (the starting available characters)
alphabet :: [Char]
alphabet = "abcdefghijklmnopqrstuvwxyz"

-----------------------------------------------------------------

-- Part A: Helper functions

showStatus :: [Status] -> String 
showStatus [] = ""
showStatus (x : xs) 
    | x == Here = "Y " ++ showStatus xs
    | x == Nowhere = "- " ++ showStatus xs
    | x == Elsewhere (Nothing) = "y " ++ showStatus xs
    | x == Elsewhere (Just Before) = "< " ++ showStatus xs
    | x == Elsewhere (Just After) = "> " ++ showStatus xs

updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable available guess = do
    let charsNowhere = nowhereChars guess
    removeSubstring charsNowhere available

leftMargin :: String 
leftMargin = take (length(prompt Start)) $ repeat ' '
 
-- Part B: Core functionality

checkGuess :: String -> String -> [(Char, Status)]
checkGuess guess word = checkGuessAux guess word word

-- Implements the main functionality of 'checkGuess'
checkGuessAux :: String -> String -> String -> [(Char, Status)]
checkGuessAux "" "" _ = []
checkGuessAux (x : xs) (y : ys) aux 
    | x == y = [(x, Here)] ++ checkGuessAux xs ys aux 
    | elem x aux == True = [(x, Elsewhere Nothing)] ++ checkGuessAux xs ys aux
    | otherwise = [(x, Nowhere)] ++ checkGuessAux xs ys aux

-- Part C: User interface - receiving guesses

getGuess :: Int -> String -> IO String
getGuess 0 _ = return ""
getGuess x available = do {
    inp <- getChar';
    if notElem inp available then do {
        putChar '\b';
        getGuess x available
    }
    else do {
            putChar ' ';
            recursiveNext <- getGuess (x - 1) available;
            when (x == 1) (putChar '\n'); -- prints a single line break before returning the guess
            return (toLower(inp) : recursiveNext);
        }
    }

-- Part D: User interface - overall gameplay

loop :: String -> [Char] -> Int -> IO ()
loop _ _ 0 = putStrLn (prompt Lose)
loop word available x = do
    putStrLn ("Attempt " ++ (show (7 - x)))
    putStrLn (prompt Start)
    guessOrQuit <- getChar'
    if guessOrQuit == 'q' then do {
        putStrLn ("\b" ++ prompt Quit);
        exitSuccess; -- try to do something different to this?
    }
    else putStr "\b"
    putStrLn (prompt Guess)
    putStr leftMargin
    guess <- getGuess 5 available
    let checkedGuess = checkGuess guess word
    let statuses = getGuessStatuses(checkedGuess)
    putStr leftMargin
    putStrLn (showStatus statuses)
    if checkWin statuses == True then putStrLn (prompt Win) else (loop word (updateAvailable available checkedGuess) (x - 1))

go :: String -> IO ()
go word = do 
    word <- wordBasedOnDay
    loop word alphabet 6

-- Part E: Extension

readWordFile :: IO [String]
readWordFile = do
    let file = readFile "wordList.txt"
    fmap lines file

getCurrentDay :: IO DayOfMonth
getCurrentDay = do 
    time <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
    return day

wordBasedOnDay :: IO String
wordBasedOnDay = do
    wordFile <- readWordFile
    index <- getCurrentDay
    let wordAtIndex = (wordFile !! (index - 1))
    return wordAtIndex

main :: IO ()
main = go undefined 