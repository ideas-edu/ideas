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

data Pos = Pos { line :: !Int, column :: !Int }
   deriving (Eq, Ord)

-- position field in last position, needed for ranged parsing
data Token 
   = TokenConId   String Pos
   | TokenVarId   String Pos
   | TokenKeyword String Pos
   | TokenSpecial Char   Pos
   | TokenString  String Pos
   | TokenInt     Int    Pos
   | TokenReal    Double Pos
 deriving (Eq, Ord)

instance Show Pos where
   show (Pos l c) = "(" ++ show l ++ "," ++ show c ++ ")"

instance Show Token where
   show token = 
      case token of
         TokenConId   s _ -> "identifier " ++ s
         TokenVarId   s _ -> "identifier " ++ s
         TokenKeyword s _ -> "keyword " ++ s
         TokenSpecial c _ -> "symbol " ++ [c]
         TokenString  s _ -> "string " ++ show s
         TokenInt     i _ -> "integer " ++ show i
         TokenReal    d _ -> "floating-point number " ++ show d

tokenPosition :: Token -> Pos
tokenPosition token =
   case token of
      TokenConId   _ p -> p
      TokenVarId   _ p -> p
      TokenKeyword _ p -> p
      TokenSpecial _ p -> p
      TokenString  _ p -> p
      TokenInt     _ p -> p
      TokenReal    _ p -> p

minPos, maxPos :: Pos
minPos = Pos minBound minBound
maxPos = Pos maxBound maxBound

minString, maxString :: String
minString = []
maxString = replicate 100 maxBound

minDouble, maxDouble :: Double
minDouble = -(10^500) -- -Infinity
maxDouble = 10^500    -- Infinity

pVarid, pConid, pString :: UU.IsParser p Token => p String
pInt :: UU.IsParser p Token => p Int
pReal   :: UU.IsParser p Token => p Double

pKey  :: UU.IsParser p Token => String -> p String 
pSpec :: UU.IsParser p Token => Char -> p Char

pKey t  = t UU.<$ TokenKeyword t minPos UU.<..> TokenKeyword t maxPos
pSpec t = t UU.<$ TokenSpecial t minPos UU.<..> TokenSpecial t maxPos
pVarid = (\(TokenVarId s _) -> s) UU.<$> TokenVarId minString minPos UU.<..> TokenVarId maxString maxPos
pConid = (\(TokenConId s _) -> s) UU.<$> TokenConId minString minPos UU.<..> TokenConId maxString maxPos
pInt = (\(TokenInt i _) -> i) UU.<$> TokenInt minBound minPos UU.<..> TokenInt maxBound maxPos
pReal  = (\(TokenReal d _) -> d) UU.<$> TokenReal minDouble minPos UU.<..> TokenReal maxDouble maxPos
pString = (\(TokenString s _) -> s) UU.<$> TokenString minString minPos UU.<..> TokenString maxString maxPos

pParens p = pSpec '(' UU.*> p UU.<* pSpec ')'
pBracks p = pSpec '[' UU.*> p UU.<* pSpec ']'
pCurly  p = pSpec '{' UU.*> p UU.<* pSpec '}'
   
instance UU.Symbol Token

scanWith :: Scanner -> String -> [Token]
scanWith scanner = rec (Pos 1 1)
 where
   rec _ [] = []
   rec pos (x:rest) 
      | isSpace x && x `notElem` specialCharacters scanner = rec (advance [x] pos) rest
      | isUpper x = let (xs, ys) = break (not . isAlphaNum) rest
                        newp     = advance (x:xs) pos
                    in if (x:xs) `elem` keywords scanner
                       then TokenKeyword (x:xs) pos : rec newp ys
                       else TokenConId (x:xs) pos : rec newp ys
      | isLower x = let (xs, ys) = break (\c -> not (isAlphaNum c) || c `elem` specialCharacters scanner) rest
                        newp     = advance (x:xs) pos
                    in if (x:xs) `elem` keywords scanner
                       then TokenKeyword (x:xs) pos : rec newp ys
                       else TokenVarId (x:xs) pos : rec newp ys
      | isDigit x || (unaryMinus scanner && x == '-' && not (null rest) && isDigit (head rest))
                  = let (xs, ys) = break (not . isDigit) rest
                    in case ys of
                          ('.':a:as) | isDigit a -> -- to do: scientific notation floating-points
                             let (bs, cs) = break (not . isDigit) as
                                 newp = advance (x:xs++('.':a:bs)) pos 
                             in TokenReal (read (x:xs++('.':a:bs))) pos : rec newp cs 
                          _ -> let n = if x=='-'
                                       then negate (fromIntegral (fromJust (readInt xs)))
                                       else fromIntegral (fromJust (readInt (x:xs)))
                                   newp = advance (x:xs) pos
                               in TokenInt n pos : rec newp ys
      | x == '"' = case scanString pos rest of
                      Just (s, newp, xs) -> TokenString s pos : rec newp xs
                      Nothing -> TokenSpecial x pos : rec (advance [x] pos) rest
      | x `elem` specialCharacters scanner = 
                    if [x] `elem` keywordOperators scanner
                    then TokenKeyword [x] pos : rec (advance [x] pos) rest
                    else TokenSpecial x pos : rec (advance [x] pos) rest
      | x `elem` operatorCharacters scanner = let (xs, ys) = break (\c -> c `elem` specialCharacters scanner || c `notElem` operatorCharacters scanner) rest
                           in if [x] `elem` keywordOperators scanner
                              then TokenKeyword [x] pos : rec (advance [x] pos) rest
                              else TokenKeyword (x:xs) pos : rec (advance (x:xs) pos) ys
      | otherwise = TokenSpecial x pos : rec (advance [x] pos) rest

advance :: String -> Pos -> Pos
advance [] p = p
advance (x:xs) p 
   | x == '\n' = advance xs p { column = column p + 1 }
   | otherwise = advance xs p { line = line p + 1, column = 1 }

scanString :: Pos -> String -> Maybe (String, Pos, String)
scanString _ [] = Nothing
scanString p ('\\':x:xs) = fmap (\(s,np,t) -> (x:s,np,t)) (scanString (advance ['\\',x] p) xs)
scanString p ('"':xs) = Just ("",advance ['"'] p,xs)
scanString p (x:xs) = fmap (\(s,np, t) -> (x:s,np,t)) (scanString (advance [x] p) xs)

----------------------------------------------------------
-- Scaning

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
         Unexpected t      -> "Unexpected " ++ show t
         ParNotClosed t    -> "Opening parenthesis " ++ show t ++ " is not closed"
         ParNoOpen t       -> "Closing parenthesis " ++ show t ++ " has no matching symbol"
         ParMismatch t1 t2 -> "Opening parenthesis " ++ show t1 ++ " is closed with " ++ show t2
         ErrorMessage msg  -> msg

errorToPositions :: SyntaxError -> [Pos]
errorToPositions err = 
   case err of
      Unexpected t      -> [tokenPosition t]
      ParNotClosed t    -> [tokenPosition t]
      ParNoOpen t       -> [tokenPosition t]
      ParMismatch t1 t2 -> [tokenPosition t1, tokenPosition t2]
      ErrorMessage _    -> []

-----------------------------------------------------------
--- Analyzing parentheses

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
isOpening (TokenSpecial ('(') _) = True
isOpening _ = False
isClosing (TokenSpecial (')') _) = True
isClosing _ = False
            
match :: Token -> Token -> Bool
match (TokenSpecial ('(') _) (TokenSpecial (')') _) = True
match _ _ = False

------------------------------------------------- 