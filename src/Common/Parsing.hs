{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- 
--
-----------------------------------------------------------------------------
module Common.Parsing 
   ( -- * Scaning
     Scanner(..), defaultScanner, makeCharsSpecial, scan, scanWith
     -- * Parsing
   , Parser, CharParser, TokenParser, parse, pChoice
     -- UU
   , Range(..), Ranged(..), leaf, pKey, pVarid
   , Associativity(..), OperatorTable, unaryOp, binaryOp, pOperators, pParens
   , Token, Pos
   , Message, (<*>), (<|>), (<$>), (<$), (*>), (<*), pChainl, pChainr, pList, pList1, opt
   ) where

import UU.Parsing hiding (Parser, parse)
import UU.Scanner hiding (scan, pParens, pKey, pVarid)
import qualified UU.Parsing as UU
import qualified UU.Scanner as UU
import Data.List
import Data.Maybe

----------------------------------------------------------
-- Scaning

data Scanner = Scanner
   { fileName           :: Maybe String
   , keywords           :: [String]
   , keywordOperators   :: [String]
   , specialCharacters  :: [Char]
   , operatorCharacters :: [Char]
   }

defaultScanner :: Scanner
defaultScanner = Scanner
   { fileName           = Nothing
   , keywords           = []
   , keywordOperators   = []
   , specialCharacters  = "(),;[]`{}"              -- Haskell's special characters 
   , operatorCharacters = "!#$%&*+./<=>?@\\^|-~"   -- The non-special characters
   }

makeCharsSpecial :: String -> Scanner -> Scanner
makeCharsSpecial cs scanner = scanner
   { specialCharacters  = specialCharacters scanner `union` cs
   , operatorCharacters = operatorCharacters scanner \\ cs
   }

scan :: String -> [Token]
scan = scanWith defaultScanner

scanWith :: Scanner -> String -> [Token]
scanWith scanner = 
   let pos = initPos $ fromMaybe "input" (fileName scanner)
   in UU.scan (keywords scanner) (keywordOperators scanner) 
              (specialCharacters scanner) (operatorCharacters scanner) pos 
                      
----------------------------------------------------------
-- Parsing

newtype Parser s a = P { unP :: AnaParser [s] Pair s (Maybe s) a }

type CharParser  = Parser Char
type TokenParser = Parser Token

instance (Symbol s, Ord s) => IsParser (Parser s) s where
   (<*>)      = liftP2 (<*>)
   (<* )      = liftP2 (<* )
   ( *>)      = liftP2 ( *>)
   (<|>)      = liftP2 (<|>) 
   (<$>)      = liftPr (<$>)
   (<$)       = liftPr (<$ ) 
   pSucceed   = P . pSucceed
   pFail      = P pFail
   pLow       = P . pLow
   pSym       = P . pSym
   pRange     = liftF2 pRange
   pCostRange = liftF3 pCostRange
   pCostSym   = liftF3 pCostSym
   getfirsts  = getfirsts . unP
   setfirsts  = \e -> P . setfirsts e . unP
   getzerop   = fmap P . getzerop . unP
   getonep    = fmap P . getonep  . unP 

-- local helper functions
liftP2 f ~(P p) ~(P q) = P (f p q)
liftPr f a ~(P p) = P (f a p)
liftF2 f a b = P (f a b)
liftF3 f a b c = P (f a b c)

parse :: Symbol s => Parser s a -> [s] -> (a, [Message s (Maybe s)])
parse (P p) input = (result, messages)
 where
   steps    = UU.parse p input
   result   = fstPair (evalSteps steps)
   messages = getMsgs steps
   fstPair (Pair a b) = a

pChoice :: (Ord s, Symbol s) => [Parser s a] -> Parser s a
pChoice = foldr (<|>) pFail

pParens :: TokenParser (Ranged a) -> TokenParser (Ranged a)
pParens p = (\p1 r p2 -> Ranged (fromRanged r) (R (p1, advc 1 p2)) True [r]) <$> pOParenPos <*> p <*> pCParenPos

newtype Range = R (Pos, Pos)

instance Eq Pos where 
  p1 == p2 = (column p1, line p1) == (column p2, line p2)
  
instance Ord Pos where
   p1 `compare` p2 = (column p1, line p1) `compare` (column p2, line p2)
   
(&) :: Range -> Range -> Range
R (p1, p2) & R (p3, p4) = R (p1 `min` p3, p2 `max` p4)

data Ranged a = Ranged 
   { fromRanged :: a 
   , getRange   :: Range
   , special    :: Bool
   , children   :: [Ranged a]
   }  

leaf :: a -> Range -> Ranged a
leaf a r = Ranged a r False []
 
subs (Ranged a r _ rs) = (a, r) : concatMap subs rs

data Associativity = LeftAssociative | RightAssociative | NonAssociative
type OperatorTable a = [(Associativity, [(String, a -> a -> a)])]

pChain :: (Ord s, Symbol s) => Associativity -> Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChain a p q = case a of
                  LeftAssociative  -> pChainl p q
                  RightAssociative -> pChainr p q
                  NonAssociative   -> (flip ($)) <$> q <*> p <*> q
                  
pOperators :: OperatorTable a -> TokenParser (Ranged a) -> TokenParser (Ranged a)
pOperators table p = foldr op p table 
 where op (a, ops) q = pChain a (pChoice $ map f ops) q
       f (s, g) = binaryOp g <$ pKey s

binaryOp :: (a -> a -> a) -> Ranged a -> Ranged a -> Ranged a       
binaryOp f r1 r2 = Ranged (f (fromRanged r1) (fromRanged r2)) (getRange r1 & getRange r2) False [r1, r2]

unaryOp :: (a -> a) -> Range -> Ranged a -> Ranged a
unaryOp f r1 r2 = Ranged (f $ fromRanged r2) (r1 & getRange r2) False [r2]

pKey :: String -> TokenParser Range
pKey   s = (\p -> R (p, advc 1 p)) <$> pKeyPos s

pVarid :: TokenParser (String, Range)
pVarid = (\(s, p) -> (s, R (p, advc 1 p))) <$> pVaridPos