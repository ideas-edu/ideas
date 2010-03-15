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
module Domain.LinearAlgebra.Parser 
   ( parseMatrix, parseVectorSpace, ppMatrix, ppMatrixWith
   , parseSystem
   ) where

import Domain.Math.Data.Relation
import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearView (isLinear)
import Domain.LinearAlgebra.Vector
import Control.Monad
import Data.List
import Data.Char
import Domain.Math.Expr
import Domain.Math.Expr.Parser
import Text.Parsing

{-
testje = case parseSystem " \n\n x == 43 \n 3*y == sqrt 4 \n" of -- "\n\n 1*x + 3*y + 2 + 87 == 2  \n   " of
            this -> this -}

parseSystem :: String -> (LinearSystem Expr, [String])
parseSystem = f . parse pSystem . scanWith s
 where
   s0 = specialSymbols "\n" scannerExpr
   s  = s0 {keywordOperators = "==" : keywordOperators s0 }
   f (Nothing, xs) = ([], "System is not linear" : map show xs)
   f (Just m, xs)  = (m, map show xs)

pSystem :: TokenParser (Maybe (LinearSystem Expr))
pSystem = convertSystem <$> pEquations pExpr
 where
   convertSystem :: Equations Expr -> Maybe (LinearSystem Expr)
   convertSystem eqs 
      | all f eqs = return eqs
      | otherwise = Nothing
    where 
       f (a :==: b) = isLinear a && isLinear b
 
-----------------------------------------------------------
--- Parser

parseMatrix :: String -> (Matrix Expr, [String])
parseMatrix = f . parse p . scanWith s
 where
   s = specialSymbols "\n" scannerExpr
   p = pMatrix pFractional
   f (Nothing, xs) = (makeMatrix [], "Matrix is not rectangular" : map show xs)
   f (Just m, xs)  = (m, map show xs)

pMatrix :: TokenParser a -> TokenParser (Maybe (Matrix a))
pMatrix p = make <$> pLines True (pList1 p)
 where 
   make xs = if isRectangular xs then Just (makeMatrix xs) else Nothing 

parseVectorSpace :: String -> (VectorSpace Expr, [Message Token])
parseVectorSpace = parse p . scanWith s
 where
   s = specialSymbols "\n" scannerExpr
   p = makeVectorSpace <$> pVectors pExpr

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
              in unlines $ map (unwords . zipWith align ws) m