{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Base where

-- Following imports needed for the `getChar'` fix
import Data.Char
import Foreign.C.Types

-- Describe the status of each letter in the guess
data Status =
    Here                         -- Letter in correct place
  | Elsewhere (Maybe Direction)  -- Letter elsewhere in word, with optional direction
  | Nowhere                      -- Letter is not in the word
    deriving (Show, Eq)

-- Describe the position of a letter in a word
-- `Before` represents before and/or after
-- `After` represents strictly after
data Direction = Before | After
    deriving (Show, Eq)

-- Names of various prompts
data Prompt = Guess | Start | Quit | Lose | Win
    deriving (Show, Eq, Enum)

-- Give the string associated with the prompt
prompt :: Prompt -> String
prompt Lose  = "Run out of guesses!"
prompt Win   = "You got it. Well done!"
prompt Start = "Guess [any] or quit [q]? "
prompt Guess = "Ok. Enter your guess:    "
prompt Quit  = "Bye!"

-----------------------------------------------------------
-- No need to read past here ------------------------------
-----------------------------------------------------------

-- Multi-platform version of `getChar` which has a fix for a GHC bug with Windows cmd/Powershell
getChar' :: IO Char
getChar' = do
#ifdef mingw32_HOST_OS
      -- Windows has to do things...
      c <- c_getch
      let c' = chr . fromEnum $ c
      putChar c'
      return c'
#else
    -- Linux, Unix, Mac OS X can just use the normal getChar
    getChar
#endif

#ifdef mingw32_HOST_OS  
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#endif