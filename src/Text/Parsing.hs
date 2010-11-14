{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
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
-- A simplified interface to the UU.Parsing library.
--
-----------------------------------------------------------------------------
module Text.Parsing 
   ( -- * Scaning
     module Text.Scanning
     -- * Parsing
   , Parser, CharParser, TokenParser
   , parse, parseWith, parseWithM
     -- * Primitive token parsers
   , pVarid, pConid, pOpid, pQVarid, pQConid
   , pKey, pSpec, pInt, pReal, pString
     -- * Derived token parsers
   , pParens, pBracks, pCurly, pCommas, pLines, pInteger
     -- * UU parser combinators
   , (<$>), (<$), (<*>), (*>), (<*), (<|>), optional, pList, pList1
   , pChainl, pChainr, pChoice, pFail
    -- * Operator table (parser)
   , OperatorTable, Associativity(..), pOperators
   ) where

import Data.Maybe
import Text.Scanning
import qualified UU.Parsing as UU

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

instance UU.Symbol Token

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

parse :: UU.Symbol s => Parser s a -> [s] -> Either (Maybe s) a
parse (P p) input =
   case messages of
      []              -> Right result
      UU.Msg _ ms _:_ -> Left ms 
 where
   steps    = UU.parse p input
   messages = UU.getMsgs steps
   result   = (\(UU.Pair a _) -> a) (UU.evalSteps steps)
   
parseWith :: Scanner -> TokenParser a -> String -> Either SyntaxError a
parseWith scanner p = either f Right . parse p . scanWith scanner
 where 
    f (Just s) = Left (Unexpected s)
    f Nothing  = Left (ErrorMessage "Syntax error")

parseWithM :: Monad m => Scanner -> TokenParser a -> String -> m a
parseWithM scanner p = either (fail . show) return . parseWith scanner p

----------------------------------------------------------
-- Primitive token parsers

pVarid, pConid, pOpid :: TokenParser String
pQVarid, pQConid      :: TokenParser (String, String)
pString               :: TokenParser String
pInt                  :: TokenParser Int
pReal                 :: TokenParser Double

pKey  :: String -> TokenParser String 
pSpec :: Char   -> TokenParser Char

pVarid  = makeTokS isTokenVarId  TokenVarId
pConid  = makeTokS isTokenConId  TokenConId
pOpid   = makeTokS isTokenOpId   TokenOpId
pQVarid = makeTokT isTokenQVarId TokenQVarId
pQConid = makeTokT isTokenQConId TokenQConId
pString = makeTokS isTokenString TokenString
pInt    = makeTokN isTokenInt    TokenInt
pReal   = makeTokN isTokenReal   TokenReal
pKey    = makeTokA TokenKeyword 
pSpec   = makeTokA TokenSpecial

-- helpers
makeTokS f con = makeTok f "" (con minString) (con maxString)
makeTokT f con = makeTok f ("","") (con minString minString) (con maxString maxString)
makeTokN f con = makeTok f 0 (con minBound) (con maxBound)
makeTokA con a = makeTok (const Nothing) a (con a) (con a)

makeTok f a con1 con2 = 
   (fromMaybe a . f) UU.<$> con1 minPos UU.<..> con2 maxPos

minPos, maxPos :: Pos
minPos = Pos minBound minBound
maxPos = Pos maxBound maxBound

minString, maxString :: String
minString = []
maxString = replicate 100 maxBound

minDouble, maxDouble :: Double
minDouble = -(10^500) -- -Infinity
maxDouble = 10^500    -- Infinity

instance Bounded Double where
   minBound = minDouble
   maxBound = maxDouble

----------------------------------------------------------
-- Derived token parsers
 
pParens, pBracks, pCurly :: TokenParser a -> TokenParser a
pParens p = pSpec '(' UU.*> p UU.<* pSpec ')'
pBracks p = pSpec '[' UU.*> p UU.<* pSpec ']'
pCurly  p = pSpec '{' UU.*> p UU.<* pSpec '}'

pCommas :: TokenParser a -> TokenParser [a]
pCommas p = optional ((:) <$> p <*> pList ((\_ a -> a) <$> pSpec ',' <*> p)) []

-- | Parse lines, separated by the newline character. The boolean argument indicates whether empy lines should 
-- be accepted or not. Make sure to configure the scanner to treat newlines as special characters!
pLines :: Bool -> TokenParser a -> TokenParser [a]
pLines allowEmptyLine p = catMaybes <$> pn 
 where
   pOne | allowEmptyLine = optional (Just <$> p) Nothing
        | otherwise      = Just <$> p
   pn = (:) <$> pOne <*> pList (pSpec '\n' *> pOne)

pInteger :: TokenParser Integer
pInteger = fromIntegral <$> pInt

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
(<*) = (UU.<*)

(<|>) :: (Ord s, UU.Symbol s) => Parser s a -> Parser s a -> Parser s a
(<|>) = (UU.<|>)

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