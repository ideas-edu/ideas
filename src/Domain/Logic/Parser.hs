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
-----------------------------------------------------------------------------
module Domain.Logic.Parser
   ( parseLogic, parseLogicPars, parseLogicUnicodePars -- , parseLogicProof
   , ppLogicPars, ppLogicUnicodePars
   ) where

import Common.Algebra.Boolean
import Common.Utils (ShowString(..))
import Control.Arrow
import Control.Monad
import Domain.Logic.Formula
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

-----------------------------------------------------------
--- Parser

parseWith :: Parser a -> String -> Either String a
parseWith p = left show . runParser start () ""
 where
   start = (P.whiteSpace lexer) >> p >>= \a -> eof >> return a

parseLogic :: String -> Either String SLogic
parseLogic = parseWith (parserSLogic False False)

parseLogicUnicode :: String -> Either String SLogic
parseLogicUnicode = parseWith (parserSLogic True False)

parseLogicPars :: String -> Either String SLogic
parseLogicPars input = 
     either (Left . ambiguousOperators parseLogic input) suspiciousVariable
   $ parseWith (parserSLogic False True) input

parseLogicUnicodePars :: String -> Either String SLogic
parseLogicUnicodePars input = 
   either (Left . ambiguousOperators parseLogicUnicode input) suspiciousVariable
   $ parseWith (parserSLogic True True) input

-- generalized parser
parserSLogic :: Bool -> Bool -> Parser SLogic
parserSLogic unicode extraPars = pLogic
 where
   pLogic 
      | extraPars = liftM2 (flip ($)) atom (option id composed)
      | otherwise = buildExpressionParser table atom
   
   composed = choice
      [ reservedOp implSym  >> liftM (flip (:->:))  atom 
      , reservedOp equivSym >> liftM (flip (:<->:)) atom 
      , do xs <- many1 (reservedOp disjSym >> atom)
           return (\x -> ors (x:xs))
      , do xs <- many1 (reservedOp conjSym >> atom)
           return (\x -> ands (x:xs))
      ]
   
   atom = choice 
      [ P.reserved lexer trSym >> return T
      , P.reserved lexer flSym >> return F
      , liftM (Var . ShowString) (P.identifier lexer)
      , P.parens lexer pLogic
      , reservedOp negSym >> liftM Not atom
      ]
      
   table = 
      [ [Infix (reservedOp implSym  >> return (:->:))  AssocRight ]
      , [Infix (reservedOp conjSym  >> return (:&&:))  AssocRight ]
      , [Infix (reservedOp disjSym  >> return (:||:))  AssocRight ]
      , [Infix (reservedOp equivSym >> return (:<->:)) AssocRight ]
      ]
      
   (implSym, equivSym, conjSym, disjSym, negSym, trSym, flSym)
      | unicode   = unicodeTuple
      | otherwise = asciiTuple

lexer :: P.TokenParser a
lexer = P.makeTokenParser $ emptyDef 
   { reservedNames   = ["T", "F"]
   , reservedOpNames = ["~", "<->", "->", "||", "/\\"]
   , identStart      = lower
   , identLetter     = lower
   , opStart         = fail "" 
   , opLetter        = fail ""
   }

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

-----------------------------------------------------------
--- Helper-functions for syntax warnings

ambiguousOperators :: (String -> Either a b) -> String -> String -> String
ambiguousOperators p s err =
   let msg = "Syntax error: ambiguous use of operators (write parentheses)"
   in either (const err) (const msg) (p s)

-- Report variables 
suspiciousVariable :: SLogic -> Either String SLogic
suspiciousVariable r =
   case filter p (map fromShowString (varsLogic r)) of
      v:_ -> Left $ "Unexpected variable " ++ v
                 ++ ". Did you forget an operator?" 
      _   -> Right r
 where
   p xs = length xs > 1 && all (`elem` "pqrst") xs

-----------------------------------------------------------
--- Pretty-Printer

-- | Pretty printer that produces extra parentheses: also see parseLogicPars
ppLogicPars :: SLogic -> String
ppLogicPars = ppLogicParsGen asciiTuple

-- | Pretty printer with unicode characters
ppLogicUnicodePars :: SLogic -> String
ppLogicUnicodePars = ppLogicParsGen unicodeTuple

ppLogicParsGen :: SymbolTuple -> SLogic -> String
ppLogicParsGen (impl, equiv, conj, disj, neg, tr, fl) = 
   (\f -> f 0 "") . foldLogic alg
 where
   alg = ( pp . fromShowString, binop 3 impl, binop 3 equiv, binop 1 conj
         , binop 2 disj, nott, pp tr, pp fl
         )
   binop :: Int -> String -> (Int -> String -> String) -> (Int -> String -> String) -> Int -> String -> String
   binop prio op p q n = 
      parIf (n/=0 && (n==3 || prio/=n)) 
            (p prio . ((" "++op++" ")++) . q prio)
   pp s = const (s++)
   nott  p _ = (neg++) . p 3
   parIf b f = if b then ("("++) . f . (")"++) else f

-----------------------------------------------------------
--- Ascii symbols

type SymbolTuple = (String, String, String, String, String, String, String)

asciiTuple :: SymbolTuple
asciiTuple = (implASym, equivASym, andASym, orASym, notASym, "T", "F")

implASym, equivASym, andASym, orASym, notASym :: String
implASym  = "->"
equivASym = "<->"
andASym   = "/\\"
orASym    = "||"
notASym   = "~"
   
-----------------------------------------------------------
--- Unicode symbols

unicodeTuple :: SymbolTuple
unicodeTuple = (implUSym, equivUSym, andUSym, orUSym, notUSym, "T", "F")

implUSym, equivUSym, andUSym, orUSym, notUSym :: String
implUSym  = "\8594"
equivUSym = "\8596"
andUSym   = "\8743"
orUSym    = "\8744"
notUSym   = "\172"