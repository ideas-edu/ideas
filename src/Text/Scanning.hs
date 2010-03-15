{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
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
module Text.Scanning where
--  ( -- * Scaning
--     Scanner(..), defaultScanner, makeCharsSpecial, newlinesAsSpecial
--   , minusAsSpecial, scan, scanWith, UU.Token, SyntaxError(..)
--   , errorToPositions, checkParentheses
--   , toPosition, tokenText
--   , UU.line, UU.column, UU.Pos
--   , UU.pCommas, UU.pString, UU.pFraction, UU.pInteger
--   , UU.pCurly, UU.pCParen, UU.pBracks, UU.pOParen
--   , UU.pConid, UU.pVarid, UU.pSpec, UU.pKey
--   ) where

import Common.Utils (readInt)
import Data.List
import Data.Maybe
import Data.Char
import qualified UU.Parsing as UU

data Token = ConId String | VarId String | Key String 
           | Spec Char | String String | Int Int | Real Double
   deriving (Show, Eq, Ord)

data Pos = Pos { line :: Int, column :: Int }

pVarid, pConid, pString :: UU.IsParser p Token => p String
pInteger :: UU.IsParser p Token => p Integer
pReal   :: UU.IsParser p Token => p Double

pKey t = t UU.<$ UU.pSym (Key t)
pSpec t = t UU.<$ UU.pSym (Spec t)
pVarid = (\(VarId s) -> s) UU.<$> VarId "" UU.<..> VarId [maxBound]
pConid = (\(ConId s) -> s) UU.<$> ConId "" UU.<..> ConId [maxBound]
pInteger = (\(Int i) -> fromIntegral i) UU.<$> Int minBound UU.<..> Int maxBound
pReal  = (\(Real f) -> f) UU.<$> Real (fromIntegral (minBound::Int)) UU.<..> Real (fromIntegral (maxBound::Int))
pString = (\(String s) -> s) UU.<$> String "" UU.<..> String [maxBound]
pBracks p = UU.pSym (Spec '[') UU.*> p UU.<* UU.pSym (Spec ']')
pCurly p = UU.pSym (Spec '{') UU.*> p UU.<* UU.pSym (Spec '}')

pOParen, pCParen :: (UU.IsParser p Token) => p Token
pCParen = UU.pSym (Spec ')')
pOParen = UU.pSym (Spec '(')

tokenNoPosition :: Token -> Token
tokenNoPosition = id
   
instance UU.Symbol Token

errorToPositions :: SyntaxError -> [(Int, Int)]
errorToPositions a = []
tokenText a = show a 
toPosition = const (0,0)
checkParentheses = const Nothing

scanWith :: Scanner -> String -> [Token]
scanWith scanner = rec 
 where
   rec [] = []
   rec (x:rest) 
      | isSpace x && x `notElem` specialCharacters scanner = rec rest
      | isUpper x = let (xs, ys) = break (not . isAlphaNum) rest
                    in if (x:xs) `elem` keywords scanner
                       then Key (x:xs) : rec ys
                       else ConId (x:xs) : rec ys
      | isLower x = let (xs, ys) = break (\c -> not (isAlphaNum c) || c `elem` specialCharacters scanner) rest
                    in if (x:xs) `elem` keywords scanner
                       then Key (x:xs) : rec ys
                       else VarId (x:xs) : rec ys
      | isDigit x || (unaryMinus scanner && x == '-' && not (null rest) && isDigit (head rest))
                  = let (xs, ys) = break (not . isDigit) rest
                    in case ys of
                          ('.':a:as) | isDigit a -> -- to do: scientific notation floating-points
                             let (bs, cs) = break (not . isDigit) as
                             in Real (read (x:xs++('.':a:bs))) : rec cs 
                          _ -> let n = if x=='-'
                                       then negate (fromJust (readInt xs))
                                       else fromJust (readInt (x:xs)) 
                               in Int n : rec ys
      | x == '"' = case scanString rest of
                      Just (s, xs) -> String s : rec xs
                      Nothing -> Spec x : rec rest
      | x `elem` specialCharacters scanner = 
                    if [x] `elem` keywordOperators scanner
                    then Key [x] : rec rest
                    else Spec x : rec rest
      | x `elem` operatorCharacters scanner = let (xs, ys) = break (\c -> c `elem` specialCharacters scanner || c `notElem` operatorCharacters scanner) rest
                           in if [x] `elem` keywordOperators scanner
                              then Key [x] : rec rest
                              else Key (x:xs) : rec ys
      | otherwise = Spec x : rec rest

scanString :: String -> Maybe (String, String)
scanString [] = Nothing
scanString ('\\':x:xs) = fmap (\(s,t) -> (x:s,t)) (scanString xs)
scanString ('"':xs) = Just ("",xs)
scanString (x:xs) = fmap (\(s,t) -> (x:s,t)) (scanString xs)

-- opchars = "!#$%&*+./<=>?@\\^|-~"

{-

import qualified UU.Scanner as UU 
import qualified UU.Scanner.GenToken as UU
import UU.Scanner.GenToken
import UU.Scanner (Token, noPos)
import Data.Char
import Data.List
import Data.Maybe

----------------------------------------------------------
-- Scaning
-}
-- | Data type to configure a scanner
data Scanner = Scanner
   { fileName           :: Maybe String
   , keywords           :: [String]
   , keywordOperators   :: [String]
   , specialCharacters  :: String
   , operatorCharacters :: String
   , unaryMinus         :: Bool
   } deriving Show

-- | A default scanner configuration (using Haskell's special characters)
defaultScanner :: Scanner
defaultScanner = Scanner
   { fileName           = Nothing
   , keywords           = []
   , keywordOperators   = []
   , specialCharacters  = "(),;[]`{}"              -- Haskell's special characters 
   , operatorCharacters = "!#$%&*+./<=>?@\\^|-~"   -- The non-special characters
   , unaryMinus         = False      
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
newlinesAsSpecial = makeCharsSpecial [specialNewlinesChar, '\n']

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
scan :: String -> [Token]
scan = scanWith defaultScanner
{-
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
-}
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
{-
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
-}