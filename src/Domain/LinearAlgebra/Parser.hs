-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.LinearAlgebra.Parser 
   ( parseMatrix, parseVectors, ppMatrix, ppMatrixWith
   , parseSystem
   ) where

import Domain.Math.Equation
import Domain.Math.Parser (pEquation)
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.Vector
import Control.Monad
import Data.List
import Data.Char
import qualified Domain.Math.Expr as Expr
import Domain.Math.SExpr
import Domain.Math.Parser
import Common.Parsing

{-
testje = case parseSystem " \n\n x == 43 \n 3*y == sqrt 4 \n" of -- "\n\n 1*x + 3*y + 2 + 87 == 2  \n   " of
            this -> this -}

parseSystem :: String -> (LinearSystem SExprLin, [String])
parseSystem = f . parse pSystem . scanWith s
 where
   s0 = newlinesAsSpecial scannerExpr
   s  = s0 {keywordOperators = "==" : keywordOperators s0 }
   f (Nothing, xs) = ([], "System is not linear" : map show xs)
   f (Just m, xs)  = (m, map show xs)

pSystem :: TokenParser (Maybe (LinearSystem SExprLin))
pSystem = convertSystem <$> pEquations pExpr
 where
   convertSystem :: Equations Expr.Expr -> Maybe (LinearSystem SExprLin)
   convertSystem eqs 
      | all f simple = return simple
      | otherwise    = Nothing
    where 
       simple = map (fmap simplifyExpr) eqs
       f (a :==: b) = isLinear a && isLinear b
 
-----------------------------------------------------------
--- Parser

parseMatrix :: String -> (Matrix SExpr, [String])
parseMatrix = f . parse p . scanWith s
 where
   s = newlinesAsSpecial scannerExpr
   p = (fmap (fmap simplifyExpr)) <$> pMatrix pFractional
   f (Nothing, xs) = (makeMatrix [], "Matrix is not rectangular" : map show xs)
   f (Just m, xs)  = (m, map show xs)

pMatrix :: TokenParser a -> TokenParser (Maybe (Matrix a))
pMatrix p = make <$> pLines True (pList1 p)
 where 
   make xs = if isRectangular xs then Just (makeMatrix xs) else Nothing 

parseVectors :: String -> ([Vector SExprGS], [Message Token])
parseVectors = parse p . scanWith s
 where
   s = newlinesAsSpecial scannerExpr
   p = (map (fmap simplifyExpr)) <$> pVectors pExpr

pVectors :: TokenParser a -> TokenParser [Vector a]
pVectors p = pLines True (pVector p)

pVector :: TokenParser a -> TokenParser (Vector a)
pVector p = fromList <$> myParens (myListSep (pSpec ',') p)

myListSep :: TokenParser a -> TokenParser b -> TokenParser [b]
myListSep sep p = optional ((:) <$> p <*> pList (sep *> p)) []

myParens :: TokenParser a -> TokenParser a
myParens p = pSpec '(' *> p <* pSpec ')'

-----------------------------------------------------------
--- Pretty-Printer

ppMatrix :: Show a => Matrix a -> String
ppMatrix = ppMatrixWith show
     
ppMatrixWith :: (a -> String) -> Matrix a -> String
ppMatrixWith f = ppStringMatrix . fmap f 
        
ppStringMatrix :: Matrix String -> String
ppStringMatrix = format . rows
 where
   format m = let ws = foldr (zipWith max . map length) (repeat 0) m 
                  align i s = take i (s ++ repeat ' ')
              in unlines $ map (concat . intersperse " " . zipWith align ws) m