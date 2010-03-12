{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A simplified interface to the UU.Parsing and UU.Scanner libraries. This module
-- provides some additional functionality to determine valid sub-expressions.
--
-----------------------------------------------------------------------------
module Text.Scanning 
   ( -- * Scaning
     Scanner(..), defaultScanner, makeCharsSpecial, newlinesAsSpecial
   , minusAsSpecial, scan, scanWith, UU.Token, SyntaxError(..)
   , errorToPositions, checkParentheses
   , toPosition, tokenText

   , UU.noPos, UU.line, UU.column, UU.Position, UU.Pos
   , UU.pCommas, UU.pString, UU.pFraction, UU.pInteger
   , UU.pCurly, UU.pCParen, UU.pBracks, UU.pOParen
   , UU.pConid, UU.pVarid, UU.pSpec, UU.pKey
   ) where

import qualified UU.Scanner as UU 
import qualified UU.Scanner.GenToken as UU
import UU.Scanner.GenToken
import UU.Scanner (Token, noPos)
import Data.Char
import Data.List
import Data.Maybe

----------------------------------------------------------
-- Scaning

-- | Data type to configure a scanner
data Scanner = Scanner
   { fileName           :: Maybe String
   , keywords           :: [String]
   , keywordOperators   :: [String]
   , specialCharacters  :: String
   , operatorCharacters :: String
   }

-- | A default scanner configuration (using Haskell's special characters)
defaultScanner :: Scanner
defaultScanner = Scanner
   { fileName           = Nothing
   , keywords           = []
   , keywordOperators   = []
   , specialCharacters  = "(),;[]`{}"              -- Haskell's special characters 
   , operatorCharacters = "!#$%&*+./<=>?@\\^|-~"   -- The non-special characters      
   }

-- | Add characters to the list of special characters (and remove these from the list of operator characters)
makeCharsSpecial :: String -> Scanner -> Scanner
makeCharsSpecial cs scanner = scanner
   { specialCharacters  = specialCharacters scanner `union` cs
   , operatorCharacters = operatorCharacters scanner \\ cs
   }

-- Newline characters are mapped to "special" tokens
-- The current solution to deal with newlines is a hack: all characters '\n' in the input
-- are first mapped to '\001', and later the tokens are adapted
newlinesAsSpecial :: Scanner -> Scanner
newlinesAsSpecial = makeCharsSpecial [specialNewlinesChar]

specialNewlinesChar :: Char
specialNewlinesChar = chr 1

-- Minus characters are mapped to "special" tokens
-- The current solution to deal with minus is a hack: all characters '-' in the input
-- are first mapped to '\002', and later the tokens are adapted 
-- (since the scanner considers -- to be comment)
minusAsSpecial :: Scanner -> Scanner
minusAsSpecial = makeCharsSpecial [specialMinusChar]

specialMinusChar :: Char
specialMinusChar = chr 2

-- | Scan an input string with the default scanner configuration
scan :: String -> [UU.Token]
scan = scanWith defaultScanner

-- | Scan an input string with the given scanner configuration
scanWith :: Scanner -> String -> [UU.Token]
scanWith scanner = post . uuScan . pre
 where
   -- very special characters
   special = or [specialNewlines, specialMinus]
   specialNewlines = specialNewlinesChar `elem` specialCharacters scanner
   specialMinus    = specialMinusChar    `elem` specialCharacters scanner
   
   pre    = if special then map changeChar  else id
   post   = if special then map changeToken else id
   pos    = UU.initPos $ fromMaybe "" (fileName scanner)
   uuScan = UU.scan (keywords scanner) (keywordOperators scanner) 
               (specialCharacters scanner) (operatorCharacters scanner) pos
   
   changeChar :: Char -> Char
   changeChar c
      | c == '\n' && specialNewlines = specialNewlinesChar
      | c == '-'  && specialMinus    = specialMinusChar
      | otherwise                    = c
   
   changeToken :: UU.Token -> UU.Token
   changeToken t =
      case t of
         UU.Reserved [c] pos 
            | c == specialNewlinesChar && specialNewlines -> UU.Reserved "\n" pos
            | c == specialMinusChar    && specialMinus    -> UU.Reserved "-"  pos
         _ -> t

-----------------------------------------------------------
--- Syntax errors

data SyntaxError 
   = Unexpected Token
   | ParNotClosed Token 
   | ParNoOpen Token 
   | ParMismatch Token Token
   | ErrorMessage String

instance Show SyntaxError where
   show err = 
      case err of
         Unexpected t      -> "Unexpected " ++ show (tokenNoPosition t) 
         ParNotClosed t    -> "Opening parenthesis " ++ show (tokenNoPosition t) ++ " is not closed"
         ParNoOpen t       -> "Closing parenthesis " ++ show (tokenNoPosition t) ++ " has no matching symbol"
         ParMismatch t1 t2 -> "Opening parenthesis " ++ show (tokenNoPosition t1) ++ " is closed with " ++ show (tokenNoPosition t2)
         ErrorMessage msg  -> msg

errorToPositions :: SyntaxError -> [(Int, Int)]
errorToPositions err = 
   case err of
      Unexpected t      -> [toPosition t]
      ParNotClosed t    -> [toPosition t]
      ParNoOpen t       -> [toPosition t]
      ParMismatch t1 t2 -> [toPosition t1, toPosition t2]
      ErrorMessage _    -> []

-----------------------------------------------------------
--- Analyzing parentheses

tokenText :: Token -> String
tokenText (Reserved s _)   = "symbol " ++ s
tokenText (ValToken s v _) = show s ++ " " ++ v

toPosition :: Token -> (Int, Int)
toPosition (Reserved _ p)   = (UU.line p, UU.column p)
toPosition (ValToken _ _ p) = (UU.line p, UU.column p)

tokenNoPosition :: Token -> Token
tokenNoPosition (Reserved a _)   = Reserved a noPos
tokenNoPosition (ValToken a b _) = ValToken a b noPos

checkParentheses :: [Token] -> Maybe SyntaxError
checkParentheses = rec []
 where
   rec []    [] = Nothing
   rec (t:_) [] = Just (ParNotClosed t)
   rec stack (t:ts)
      | isOpening t =
           rec (t:stack) ts
      | isClosing t =
           case stack of
              [] -> Just (ParNoOpen t) 
              x:xs
                 | match x t -> rec xs ts
                 | otherwise -> Just (ParMismatch x t)
      | otherwise =
           rec stack ts
      
isOpening, isClosing :: Token -> Bool
isOpening (Reserved ("(") _) = True
isOpening _ = False
isClosing (Reserved (")") _) = True
isClosing _ = False
            
match :: Token -> Token -> Bool
match (Reserved ("(") _) (Reserved (")") _) = True
match _ _ = False

-------------------------------------------------