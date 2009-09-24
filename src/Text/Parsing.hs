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
module Text.Parsing 
   ( -- * Scaning
     Scanner(..), defaultScanner, makeCharsSpecial, newlinesAsSpecial, minusAsSpecial, scan, scanWith, UU.Token
     -- * Parsing
   , Parser, CharParser, TokenParser, parse, Message
     -- * UU parser combinators
   , (<$>), (<$), (<*>), (*>), (<*), (<|>), optional, pList, pList1, pChainl, pChainr, pChoice
     -- * Subexpressions
   , Ranged, Range(..), Pos(..), toRanged, fromRanged, subExpressionAt
   , pKey, pSpec, pVarid, pConid, unaryOp, binaryOp, pParens, indicesToRange
   , pInteger, pFraction, pString, pBracks, pCurly, pCommas, pLines
    -- * Operator table (parser)
   , OperatorTable, Associativity(..), pOperators
    -- * Analyzing parentheses
   , SyntaxError(..), fromMessage, errorToPositions
   , checkParentheses, showTokenPos, tokenNoPosition
   , toPosition, tokenText
   ) where

import qualified UU.Parsing as UU
import qualified UU.Scanner as UU
import qualified UU.Scanner.GenToken as UU
import Control.Arrow
import Common.Utils
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
                      
----------------------------------------------------------
-- Parsing

-- | Abstract data type for a parser, where @s@ is the symbol type, and @a@ is 
-- the result type. This data type is an instance of the @IsParser@ type class
-- defined in the UU libraries.
newtype Parser s a = P { unP :: UU.AnaParser [s] UU.Pair s (Maybe s) a }

-- | A parser with characters as symbol type
type CharParser  = Parser Char

-- | A parser with tokens as symbol type
type TokenParser = Parser UU.Token

instance (UU.Symbol s, Ord s) => UU.IsParser (Parser s) s where
   (<*>)       = liftP2 (UU.<*>)
   (<* )       = liftP2 (UU.<*)
   ( *>)       = liftP2 (UU.*>)
   (<|>)       = liftP2 (UU.<|>) 
   (<$>)       = liftPr (UU.<$>)
   (<$)        = liftPr (UU.<$ ) 
   pSucceed    = P . UU.pSucceed
   pFail       = P UU.pFail
   pLow        = P . UU.pLow
   pSym        = P . UU.pSym
   pRange      = liftF2 UU.pRange
   pCostRange  = liftF3 UU.pCostRange
   pCostSym    = liftF3 UU.pCostSym
   getfirsts   = UU.getfirsts . unP
   setfirsts e = P . UU.setfirsts e . unP
   getzerop    = fmap P . UU.getzerop . unP
   getonep     = fmap P . UU.getonep  . unP 

-- local helper functions
liftP2 f ~(P p) ~(P q) = P (f p q)
liftPr f a ~(P p) = P (f a p)
liftF2 f a   = P . f a
liftF3 f a b = P . f a b

type Message s = (UU.Expecting s, Maybe s)

-- Parsing an input string always returns a result and a list of error messages
parse :: UU.Symbol s => Parser s a -> [s] -> (a, [Message s])
parse (P p) input = (result, map f messages)
 where
   steps    = UU.parse p input
   result   = fstPair (UU.evalSteps steps)
   messages = UU.getMsgs steps
   fstPair (UU.Pair a _) = a
   f (UU.Msg a b _) = (a, b)

----------------------------------------------------------
-- UU parser combinators

infixl 3 <|>
infixl 4 <$>, <$, <*>, <*, *>

(<$>) :: (Ord s, UU.Symbol s) => (a -> b) -> Parser s a -> Parser s b
(<$>) = (UU.<$>)

(<$) :: (Ord s, UU.Symbol s) => a -> Parser s b -> Parser s a
(<$) = (UU.<$)

(<*>) :: (Ord s, UU.Symbol s) => Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = (UU.<*>)

(*>) :: (Ord s, UU.Symbol s) => Parser s a -> Parser s b -> Parser s b
(*>) = (UU.*>)

(<*) :: (Ord s, UU.Symbol s) => Parser s a -> Parser s b -> Parser s a
(<*)   a = (UU.<*) a

(<|>) :: (Ord s, UU.Symbol s) => Parser s a -> Parser s a -> Parser s a
(<|>)   a = (UU.<|>) a

optional :: (Ord s, UU.Symbol s) => Parser s a -> a -> Parser s a
optional = UU.opt

pList, pList1 :: (Ord s, UU.Symbol s) => Parser s a -> Parser s [a]
pList = UU.pList
pList1 = UU.pList1

pChainl, pChainr :: (Ord s, UU.Symbol s) => Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainl = UU.pChainl
pChainr = UU.pChainr

pChoice :: (Ord s, UU.Symbol s) => [Parser s a] -> Parser s a
pChoice = foldr (<|>) UU.pFail

----------------------------------------------------------
-- Subexpressions

-- | Abstract data type for expressions that ''know'' about the ranges of their 
-- subexpressions
data Ranged a = Ranged 
   { fromRanged :: a           -- ^ Forget about the subexpressions
   , getRange   :: Range
   , special    :: Bool
   , children   :: [Ranged a]
   } 

instance Show a => Show (Ranged a) where
   show = show . fromRanged

-- | Data type for ranges
data Range = Range
   { beginPos :: Pos
   , endPos   :: Pos
   }
 deriving (Show, Eq, Ord)

-- | Data type for positions
data Pos = Pos
   { line   :: Int
   , column :: Int
   }
 deriving (Show, Eq, Ord)

-- | A value without subexpressions
toRanged :: a -> Range -> Ranged a
toRanged a r = Ranged a r False []

-- | Given a selection (range) and a ranged term, return the location of the selected 
-- subexpression (or Nothing to indicate that the selection is invalid)
subExpressionAt :: Range -> Ranged a -> Maybe [Int]
subExpressionAt r ra
   | r == getRange ra = return []
   | otherwise = 
        let f i | special ra = id
                | otherwise  = (i:)
        in safeHead $ catMaybes
              [ fmap (f i) (subExpressionAt r c) | (i, c) <- zip [0..] (children ra) ]

pKey :: String -> TokenParser Range
pKey  s = toRange 1 <$> UU.pKeyPos  s

pSpec :: Char -> TokenParser Range
pSpec c = toRange 1 <$> UU.pSpecPos c

pVarid, pConid :: TokenParser (String, Range)
pVarid = second (toRange 1) <$> UU.pVaridPos
pConid = second (toRange 1) <$> UU.pConidPos
   
unaryOp :: (a -> a) -> Range -> Ranged a -> Ranged a
unaryOp f r1 r2 = Ranged (f $ fromRanged r2) (r1 & getRange r2) False [r2]

binaryOp :: (a -> a -> a) -> Ranged a -> Ranged a -> Ranged a       
binaryOp f r1 r2 = Ranged (f (fromRanged r1) (fromRanged r2)) (getRange r1 & getRange r2) False [r1, r2]

pParens :: TokenParser (Ranged a) -> TokenParser (Ranged a)
pParens p = (\p1 r p2 -> Ranged (fromRanged r) (toRange 1 p1 & toRange 1 p2) True [r]) <$> UU.pOParenPos <*> p <*> UU.pCParenPos

-- TODO: fix inconsistency with pParens
pBracks :: TokenParser a -> TokenParser a
pBracks  = UU.pBracks

-- | Parse lines, separated by the newline character. The boolean argument indicates whether empy lines should 
-- be accepted or not. Make sure to configure the scanner to treat newlines as special characters!
pLines :: Bool -> TokenParser a -> TokenParser [a]
pLines allowEmptyLine p = catMaybes <$> pn 
 where
   pOne | allowEmptyLine = optional (Just <$> p) Nothing
        | otherwise      = Just <$> p
   pn = (:) <$> pOne <*> pList (pSpec '\n' *> pOne)

-- TODO: fix inconsistency with pParens
pCurly :: TokenParser a -> TokenParser a
pCurly   = UU.pCurly

pInteger :: TokenParser Integer
pInteger = read <$> UU.pInteger

pFraction :: TokenParser Float
pFraction = read <$> UU.pFraction

pString :: TokenParser String
pString = UU.pString

pCommas :: TokenParser a -> TokenParser [a]
pCommas = UU.pCommas

-- | Helper function to translate two indices on a string to a range: the positions of a range are line-based
indicesToRange :: String -> Int -> Int -> Range
indicesToRange s i j = Range (indexToPos s a) (indexToPos s b) 
 where (a, b) = trimIndexPair s i j

-- local helper functions
(&) :: Range -> Range -> Range
Range p1 p2 & Range p3 p4 = Range (p1 `min` p3) (p2 `max` p4)

toPos :: UU.Pos -> Pos
toPos p = Pos (UU.line p) (UU.column p)

toRange :: Int -> UU.Pos -> Range
toRange n p = Range (toPos p) (toPos (UU.advc n p))

indexToPos :: String -> Int -> Pos
indexToPos = rec . zip [1..] . lines
 where
   rec [] _ = Pos 0 0
   rec ((lnr, x):rest) i
      | i <= len  = Pos lnr (i+1)
      | otherwise = rec rest (i-len-1)
    where
      len = length x

trimIndexPair :: String -> Int -> Int -> (Int, Int)
trimIndexPair s i j 
   | j < i     = trimIndexPair s j i
   | otherwise = (i + f sub, j - f (reverse sub))
 where 
    sub = take (j-i) (drop i s)
    f   = length . takeWhile isSpace

----------------------------------------------------------
-- Operator table (parser)

-- | Type for an operator table. Operators with a low priority should appear in the front of the list.
type OperatorTable a = [(Associativity, [(String, a -> a -> a)])]

-- | Data type to express the kind of associativity. The NoMix constructor expresses that the operators
-- in the list should not be mixed, but require extra parentheses in the input
data Associativity = LeftAssociative | RightAssociative | NonAssociative | NoMix

-- | Construct a parser using an operator table
pOperators :: OperatorTable a -> TokenParser (Ranged a) -> TokenParser (Ranged a)
pOperators table p = foldr op p table 
 where op (a, ops) q = 
          case a of
             -- The NoMix variant is actually hard to define efficiently. Since we should not mix operators
             -- that have the same priority, we have to inspect which operator we are dealing with before
             -- we can use the chain combinator.
             NoMix -> let make op = flip <$> f op <*> pChainr (f op) q
                      in flip ($) <$> q <*> optional (pChoice $ map make ops) id
             _     -> pChain a (pChoice $ map f ops) q
       f (s, g) = binaryOp g <$ pKey s


-- local helper function
pChain :: (Ord s, UU.Symbol s) => Associativity -> Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChain a p q = case a of
                  LeftAssociative  -> pChainl p q
                  RightAssociative -> pChainr p q
                  NonAssociative   -> flip ($) <$> q <*> p <*> q
                  NoMix            -> pChainr p q

-----------------------------------------------------------
--- Syntax errors

data SyntaxError 
   = Unexpected UU.Token
   | ParNotClosed UU.Token 
   | ParNoOpen UU.Token 
   | ParMismatch UU.Token UU.Token
   | ErrorMessage String

instance Show SyntaxError where
   show err = 
      case err of
         Unexpected t      -> "Unexpected " ++ show (tokenNoPosition t) 
         ParNotClosed t    -> "Opening parenthesis " ++ show (tokenNoPosition t) ++ " is not closed"
         ParNoOpen t       -> "Closing parenthesis " ++ show (tokenNoPosition t) ++ " has no matching symbol"
         ParMismatch t1 t2 -> "Opening parenthesis " ++ show (tokenNoPosition t1) ++ " is closed with " ++ show (tokenNoPosition t2)
         ErrorMessage msg  -> msg

fromMessage :: Message UU.Token -> SyntaxError
fromMessage (_, Just t) = Unexpected t
fromMessage _           = ErrorMessage "Syntax error"

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

tokenText :: UU.Token -> String
tokenText (UU.Reserved s _)   = "symbol " ++ s
tokenText (UU.ValToken s v _) = show s ++ " " ++ v

showTokenPos :: UU.Token -> String
showTokenPos (UU.Reserved _ p)   = showPosition p
showTokenPos (UU.ValToken _ _ p) = showPosition p

toPosition :: UU.Token -> (Int, Int)
toPosition (UU.Reserved _ p)   = (UU.line p, UU.column p)
toPosition (UU.ValToken _ _ p) = (UU.line p, UU.column p)

showPosition :: UU.Position a => a -> String
showPosition p = show (UU.line p, UU.column p)

tokenNoPosition :: UU.Token -> UU.Token
tokenNoPosition (UU.Reserved a _)   = UU.Reserved a UU.noPos
tokenNoPosition (UU.ValToken a b _) = UU.ValToken a b UU.noPos

checkParentheses :: [UU.Token] -> Maybe SyntaxError
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
      
isOpening, isClosing :: UU.Token -> Bool
isOpening (UU.Reserved ("(") _) = True
isOpening _ = False
isClosing (UU.Reserved (")") _) = True
isClosing _ = False
            
match :: UU.Token -> UU.Token -> Bool
match (UU.Reserved ("(") _) (UU.Reserved (")") _) = True
match _ _ = False