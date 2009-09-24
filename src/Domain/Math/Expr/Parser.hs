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
-----------------------------------------------------------------------------
module Domain.Math.Expr.Parser 
   ( scannerExpr, parseExpr, parseWith, pExpr
   , pEquations, pEquation, pOrList, pFractional
   ) where

import Prelude hiding ((^))
import Text.Parsing hiding (pParens)
import Control.Monad
import Data.List
import Common.Transformation
import Domain.Math.Data.Equation
import Domain.Math.Expr.Data
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Data.OrList
import Test.QuickCheck (arbitrary)

import Text.OpenMath.Dictionary.Arith1
import Text.OpenMath.Dictionary.Logic1
import Text.OpenMath.Dictionary.Relation1
import Text.OpenMath.Dictionary.Calculus1

symbols :: [Symbol]
symbols = nubBy (\x y -> symbolName x == symbolName y) $ 
   concat [ arith1List, logic1List, relation1List, calculus1List ]

scannerExpr :: Scanner
scannerExpr = defaultScanner 
   { keywords          = "sqrt" : map symbolName symbols
   , keywordOperators  = ["==" ]
   , specialCharacters = "+-*/^()[]{},"
   }

parseWith :: TokenParser a -> String -> Either SyntaxError a
parseWith p = f . parse p . scanWith scannerExpr
 where 
   f (e, []) = Right e
   f (_, xs) = Left $ ErrorMessage $ unlines $ map show xs

parseExpr :: String -> Either SyntaxError Expr
parseExpr = parseWith pExpr

pExpr :: TokenParser Expr
pExpr = expr6

-- This expression could have a fraction at top-level: both the numerator
-- and denominator are atoms, optionally preceded by a (unary) minus
pFractional :: TokenParser Expr
pFractional = expr6u -- flip ($) <$> expr6u <*> optional (flip (/) <$ pKey "/" <*> expr6u) id

expr6, expr6u, expr7, expr8, term, atom :: TokenParser Expr
expr6  =  pChainl ((+) <$ pKey "+" <|> (-) <$ pKey "-") expr6u
expr6u =  optional (Negate <$ pKey "-") id <*> expr7
expr7  =  pChainl ((*) <$ pKey "*" <|> (/) <$ pKey "/") expr8
expr8  =  pChainr ((^) <$ pKey "^") term
term   =  symb <*> pList atom
      <|> atom
atom   =  fromInteger <$> pInteger
      <|> (Var . fst) <$> pVarid
      <|> pParens pExpr

symb :: TokenParser ([Expr] -> Expr)
symb =  pChoice (map (\s -> function s <$ pKey (symbolName s)) symbols)
    -- To fix: sqrt expects exactly one argument
    <|> (\xs -> function rootSymbol (xs ++ [2])) <$ pKey "sqrt" 

pEquations :: TokenParser a -> TokenParser (Equations a)
pEquations = pLines True . pEquation

pEquation :: TokenParser a -> TokenParser (Equation a)
pEquation p = (:==:) <$> p <* pKey "==" <*> p

pOrList :: TokenParser a -> TokenParser (OrList a)
pOrList p = (join . orList) <$> pSepList pTerm (pKey "or")
 where 
   pTerm =  return <$> p 
        <|> true   <$  pKey "true" 
        <|> false  <$  pKey "false"
   pSepList p q = (:) <$> p <*> pList (q *> p)

pParens :: TokenParser a -> TokenParser a
pParens p = pKey "(" *> p <* pKey ")"

-----------------------------------------------------------------------
-- Argument descriptor (for parameterized rules)

instance Argument Expr where
   makeArgDescr = exprArgDescr

exprArgDescr :: String -> ArgDescr Expr
exprArgDescr descr = ArgDescr descr Nothing (either (const Nothing) Just . parseExpr) show arbitrary