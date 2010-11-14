-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A simple scanner with some configuration facilities
--
-----------------------------------------------------------------------------
module Text.Scanning 
   ( -- Data types
     Pos(..), Token(..)
     -- Token selectors
   , isTokenConId, isTokenVarId, isTokenOpId, isTokenQConId
   , isTokenQVarId, isTokenKeyword, isTokenSpecial
   , isTokenString, isTokenInt, isTokenReal, tokenPosition
     -- Scanner configuration
   , Scanner(..), defaultScanner, specialSymbols  
     -- Scanning
   , scan, scanWith, scanInt, scanNumber
     -- Lexical analysis
   , SyntaxError(..), checkParentheses, errorPositions
   ) where

import Control.Monad
import Data.List
import Data.Char

----------------------------------------------------------
-- * Data types

data Pos = Pos { line :: !Int, column :: !Int }
   deriving (Eq, Ord)

-- position field in last position, needed for ranged parsing
data Token 
   = TokenConId   String Pos
   | TokenVarId   String Pos
   | TokenOpId    String Pos
   | TokenQConId  String String Pos
   | TokenQVarId  String String Pos
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
         TokenConId    s _ -> "identifier " ++ s
         TokenVarId    s _ -> "identifier " ++ s
         TokenOpId     s _ -> "operator " ++ s
         TokenQConId q s _ -> "identifier " ++ q ++ "." ++ s
         TokenQVarId q s _ -> "identifier " ++ q ++ "." ++ s
         TokenKeyword  s _ -> "keyword " ++ s
         TokenSpecial  c _ -> "symbol " ++ [c]
         TokenString   s _ -> "string " ++ show s
         TokenInt      i _ -> "integer " ++ show i
         TokenReal     d _ -> "floating-point number " ++ show d

----------------------------------------------------------
-- * Token selectors

isTokenConId :: Token -> Maybe String
isTokenConId (TokenConId s _) = Just s
isTokenConId _                = Nothing

isTokenVarId :: Token -> Maybe String
isTokenVarId (TokenVarId s _) = Just s
isTokenVarId _                = Nothing

isTokenOpId :: Token -> Maybe String
isTokenOpId (TokenOpId s _) = Just s
isTokenOpId _               = Nothing

isTokenQConId :: Token -> Maybe (String, String)
isTokenQConId (TokenQConId q s _) = Just (q, s)
isTokenQConId _                   = Nothing

isTokenQVarId :: Token -> Maybe (String, String)
isTokenQVarId (TokenQVarId q s _) = Just (q, s)
isTokenQVarId _                   = Nothing

isTokenKeyword :: Token -> Maybe String
isTokenKeyword (TokenKeyword s _) = Just s
isTokenKeyword _                  = Nothing

isTokenSpecial :: Token -> Maybe Char
isTokenSpecial (TokenSpecial c _) = Just c
isTokenSpecial   _                = Nothing

isTokenString :: Token -> Maybe String
isTokenString (TokenString s _) = Just s
isTokenString  _                = Nothing

isTokenInt :: Token -> Maybe Int
isTokenInt (TokenInt i _) = Just i
isTokenInt _              = Nothing

isTokenReal :: Token -> Maybe Double
isTokenReal (TokenReal d _) = Just d
isTokenReal _               = Nothing
      
tokenPosition :: Token -> Pos
tokenPosition token =
   case token of
      TokenConId    _ p -> p
      TokenVarId    _ p -> p
      TokenOpId     _ p -> p
      TokenQConId _ _ p -> p
      TokenQVarId _ _ p -> p
      TokenKeyword  _ p -> p
      TokenSpecial  _ p -> p
      TokenString   _ p -> p
      TokenInt      _ p -> p
      TokenReal     _ p -> p

----------------------------------------------------------
-- * Scanner configuration

-- | Data type to configure a scanner
data Scanner = Scanner
   { keywords              :: [String]
   , keywordOperators      :: [String]
   , isIdentifierCharacter :: Char -> Bool
   , specialCharacters     :: String
   , operatorCharacters    :: String
   , unaryMinus            :: Bool
   , qualifiedIdentifiers  :: Bool
   }

-- | A default scanner configuration (using Haskell's special characters)
defaultScanner :: Scanner
defaultScanner = Scanner
   { keywords              = []
   , keywordOperators      = []
   , isIdentifierCharacter = \c -> isAlphaNum c || c `elem` "_'"
   , specialCharacters     = "(),;[]`{}"              -- Haskell's special characters 
   , operatorCharacters    = "!#$%&*+./<=>?@\\^|-~"   -- The non-special characters
   , unaryMinus            = False      
   , qualifiedIdentifiers  = False
   }

-- | Add characters to the list of special characters (and remove these from the list of operator characters)
specialSymbols :: String -> Scanner -> Scanner
specialSymbols cs scanner = scanner
   { specialCharacters = specialCharacters scanner `union` cs }

----------------------------------------------------------
-- * Scanning

-- | Scan an input string with the default scanner configuration
scan :: String -> [Token]
scan = scanWith defaultScanner

scanWith :: Scanner -> String -> [Token]
scanWith scanner = rec (Pos 1 1)
 where 
   rec :: Pos -> String -> [Token]
   rec _ [] = []
   rec pos input@(x:rest) 
      | isSpace x = 
           let newp = advance [x] pos
           in if x `elem` specialCharacters scanner
              then TokenSpecial x pos : rec newp rest
              else rec newp rest
      | isAlpha x =
           case scanIdentifier scanner input of
              Just (Just q, s, xs) 
                 | isLower (head s) -> make TokenQVarId
                 | otherwise        -> make TokenQConId
               where
                 make f = f q s pos : rec newp xs
                 newp   = incr (length q + length s + 1) pos
              Just (Nothing, s, xs)
                 | s `elem` keywords scanner -> make TokenKeyword
                 | isLower (head s)          -> make TokenVarId 
                 | otherwise                 -> make TokenConId
               where 
                 make f = f s pos : rec newp xs
                 newp   = incr (length s) pos
              _ -> error "unexpected case in scanner"
      | isNumber input =
           case scanNumber pos input of
              Just (Left i,  newp, xs) -> TokenInt  i pos : rec newp xs
              Just (Right d, newp, xs) -> TokenReal d pos : rec newp xs
              _ -> error "unexpected case in scanner" 
      | x == '"' = 
           case scanString pos rest of
              Just (s, newp, xs) -> 
                 TokenString s pos : rec newp xs
              Nothing -> 
                 TokenSpecial x pos : rec (incr 1 pos) rest
      | x `elem` specialCharacters scanner = 
           let newp = incr 1 pos 
           in  if [x] `elem` keywordOperators scanner
               then TokenKeyword [x] pos : rec newp rest
               else TokenSpecial x pos : rec newp rest
      | x `elem` operatorCharacters scanner = 
           let (xs, ys) = break stop rest
               newp     = incr (length (x:xs)) pos
               stop c   =  c `elem` specialCharacters scanner 
                        || c `notElem` operatorCharacters scanner
           in if (x:xs) `elem` keywordOperators scanner
              then TokenKeyword (x:xs) pos : rec newp ys
              else TokenOpId (x:xs) pos : rec newp ys
      | otherwise = 
           let newp = incr 1 pos
           in TokenSpecial x pos : rec newp rest

   isNumber :: String -> Bool
   isNumber ('-':x:_) = isDigit x && unaryMinus scanner
   isNumber (x:_)     = isDigit x
   isNumber _         = False

scanIdentifier :: Scanner -> String -> Maybe (Maybe String, String, String)
scanIdentifier scanner (x:rest) | isAlpha x = 
   case span (isIdentifierCharacter scanner) rest of
      (xs, '.':y:rest2) | qualifiedIdentifiers scanner && isAlpha y -> 
         let (ys, zs) = span (isIdentifierCharacter scanner) rest2
         in Just (Just (x:xs), y:ys, zs)
      (xs, ys) -> 
         Just (Nothing, x:xs, ys)
scanIdentifier _ _ = Nothing

scanNumber :: Pos -> String -> Maybe (Either Int Double, Pos, String)
scanNumber pos input = do
   (i, p, xs) <- scanInt pos input
   case fractionPart p xs of
      Just (p1, ys) -> 
         case powerPart p1 ys of
            Just (p2, zs) -> 
               let txt = take (column p2 - column pos) input
               in return (Right (read txt), p2, zs)
            Nothing ->
               let txt = take (column p1 - column pos) input 
               in return (Right (read txt), p1, ys)
      Nothing -> 
         case powerPart p xs of   
            Just (p1, ys) -> 
               let txt = take (column p1 - column pos) input
               in return (Right (read txt), p1, ys)
            Nothing -> 
               return (Left i, p, xs)

fractionPart :: Pos -> String -> Maybe (Pos, String)
fractionPart pos ('.':rest) = do
   (_, p, ys) <- scanNatural pos rest
   return (incr 1 p, ys)
fractionPart _ _ = Nothing

powerPart :: Pos -> String -> Maybe (Pos, String)
powerPart pos (s:rest) | s `elem` "eE" = do
   (_, p, ys) <- scanInt pos rest
   return (incr 1 p, ys)
powerPart _ _ = Nothing

scanInt :: Pos -> String -> Maybe (Int, Pos, String)
scanInt pos ('-':xs) =
   do (nat, p, rest) <- scanNatural pos xs
      return (-nat, incr 1 p, rest)
scanInt pos xs = 
   scanNatural pos xs

scanNatural :: Pos -> String -> Maybe (Int, Pos, String)
scanNatural pos input = do
   let (xs, ys) = span isDigit input
   guard (not (null xs))
   let nat = foldl' (\a b -> a*10+ord b-48) 0 xs
   return (nat, incr (length xs) pos, ys)

scanString :: Pos -> String -> Maybe (String, Pos, String)
scanString pos input =
   case input of 
      []        -> Nothing 
      '\\':x:xs -> add x (scanString (incr 2 pos) xs)
      '"':xs    -> Just ("",incr 1 pos,xs)
      x:xs      -> add x (scanString (incr 1 pos) xs)
 where
   add c = fmap (\(s, np, t) -> (c:s, np, t))
   
advance :: String -> Pos -> Pos
advance [] = id
advance (x:xs)  
   | x == '\n' = advance xs . nextline
   | otherwise = advance xs . incr 1

incr :: Int -> Pos -> Pos
incr i p = p { column = column p + i }

nextline :: Pos -> Pos
nextline p = p { line = line p + 1, column = 1 }

-----------------------------------------------------------
--- Lexical analysis

data SyntaxError 
   = Unexpected Token
   | ParNotClosed Token 
   | ParNoOpen Token 
   | ParMismatch Token Token
   | ErrorMessage String

instance Show SyntaxError where
   show err = prefixPosition err ++
      case err of
         Unexpected t      -> "Unexpected " ++ show t
         ParNotClosed t    -> "Opening parenthesis " ++ show t ++ " is not closed"
         ParNoOpen t       -> "Closing parenthesis " ++ show t ++ " has no matching symbol"
         ParMismatch t1 t2 -> "Opening parenthesis " ++ show t1 ++ " is closed with " ++ show t2
         ErrorMessage msg  -> msg

prefixPosition :: SyntaxError -> String
prefixPosition err
   | null xs   = ""
   | otherwise = concat (intersperse "," xs) ++ ": "
 where
   xs = map show (errorPositions err)

errorPositions :: SyntaxError -> [Pos]
errorPositions err = 
   case err of
      Unexpected t      -> [tokenPosition t]
      ParNotClosed t    -> [tokenPosition t]
      ParNoOpen t       -> [tokenPosition t]
      ParMismatch t1 t2 -> [tokenPosition t1, tokenPosition t2]
      ErrorMessage _    -> []

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