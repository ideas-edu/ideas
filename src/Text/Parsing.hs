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
     Scanner(..), defaultScanner, makeCharsSpecial, newlinesAsSpecial
   , minusAsSpecial, scan, scanWith, Token
     -- * Parsing
   , Parser, CharParser, TokenParser, parse, Message
     -- * UU parser combinators
   , (<$>), (<$), (<*>), (*>), (<*), (<|>), optional, pList, pList1
   , pChainl, pChainr, pChoice, pFail
     -- * Subexpressions
   , Pos(..)
   , pKey, pSpec, pVarid, pConid, pParens
   , pInteger, pFraction, pString, pBracks, pCurly, pCommas, pLines
    -- * Operator table (parser)
   , OperatorTable, Associativity(..), pOperators
    -- * Analyzing parentheses
   , SyntaxError(..), fromMessage, errorToPositions
   , checkParentheses 
   , toPosition, tokenText
   ) where

import qualified UU.Parsing as UU
import Common.Utils
import Data.Char
import Data.List
import Data.Maybe
import Text.Scanning hiding (column, line, Pos, pCommas, pCurly, pBracks, pString, pFraction, pInteger)
import qualified Text.Scanning as UU

----------------------------------------------------------
-- Parsing

-- | Abstract data type for a parser, where @s@ is the symbol type, and @a@ is 
-- the result type. This data type is an instance of the @IsParser@ type class
-- defined in the UU libraries.
newtype Parser s a = P { unP :: UU.AnaParser [s] UU.Pair s (Maybe s) a }

-- | A parser with characters as symbol type
type CharParser  = Parser Char

-- | A parser with tokens as symbol type
type TokenParser = Parser Token

instance (UU.Symbol s, Ord s) => UU.IsParser (Parser s) s where
   ~(P p) <*>  ~(P q)  = P (p UU.<*> q)
   ~(P p) <*   ~(P q)  = P (p UU.<*  q)
   ~(P p)  *>  ~(P q)  = P (p  UU.*> q)
   ~(P p) <|>  ~(P q)  = P (p UU.<|> q) 
   a      <$>  ~(P p)  = P (a UU.<$> p)
   a      <$   ~(P p)  = P (a UU.<$  p) 
   pSucceed            = P . UU.pSucceed
   pFail               = P UU.pFail
   pLow                = P . UU.pLow
   pSym                = P . UU.pSym
   pRange a            = P . UU.pRange a
   pCostRange a b      = P . UU.pCostRange a b
   pCostSym a b        = P . UU.pCostSym a b
   getfirsts           = UU.getfirsts . unP
   setfirsts e         = P . UU.setfirsts e . unP
   getzerop            = fmap P . UU.getzerop . unP
   getonep             = fmap P . UU.getonep  . unP 

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

pFail :: (Ord s, UU.Symbol s) => Parser s a
pFail = UU.pFail

----------------------------------------------------------
-- Subexpressions

-- | Data type for positions
data Pos = Pos
   { line   :: Int
   , column :: Int
   }
 deriving (Show, Eq, Ord)

pParens :: TokenParser a -> TokenParser a
pParens p = UU.pOParen *> p <* UU.pCParen

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
pInteger = (maybe 0 fromIntegral . readInt) <$> UU.pInteger

pFraction :: TokenParser Float
pFraction = read <$> UU.pFraction

pString :: TokenParser String
pString = UU.pString

pCommas :: TokenParser a -> TokenParser [a]
pCommas = UU.pCommas

----------------------------------------------------------
-- Operator table (parser)

-- | Type for an operator table. Operators with a low priority should appear in the front of the list.
type OperatorTable a = [(Associativity, [(String, a -> a -> a)])]

-- | Data type to express the kind of associativity. The NoMix constructor expresses that the operators
-- in the list should not be mixed, but require extra parentheses in the input
data Associativity = LeftAssociative | RightAssociative | NonAssociative | NoMix

-- | Construct a parser using an operator table
pOperators :: OperatorTable a -> TokenParser a -> TokenParser a
pOperators table p = foldr op p table 
 where op (a, ops) q = 
          case a of
             -- The NoMix variant is actually hard to define efficiently. Since we should not mix operators
             -- that have the same priority, we have to inspect which operator we are dealing with before
             -- we can use the chain combinator.
             NoMix -> let make op = flip <$> f op <*> pChainr (f op) q
                      in flip ($) <$> q <*> optional (pChoice $ map make ops) id
             _     -> pChain a (pChoice $ map f ops) q
       f (s, g) = g <$ pKey s


-- local helper function
pChain :: (Ord s, UU.Symbol s) => Associativity -> Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChain a p q = case a of
                  LeftAssociative  -> pChainl p q
                  RightAssociative -> pChainr p q
                  NonAssociative   -> flip ($) <$> q <*> p <*> q
                  NoMix            -> pChainr p q

fromMessage :: Message Token -> SyntaxError
fromMessage (_, Just t) = Unexpected t
fromMessage _           = ErrorMessage "Syntax error"