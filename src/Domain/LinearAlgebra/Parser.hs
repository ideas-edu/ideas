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
   ( parseMatrix, parseVectors, ppMatrixInContext, ppMatrix, ppMatrixWith {-, ppRationalMatrix, ppRational, ppMySqrt-}
   , parseSystem
   ) where

import Domain.LinearAlgebra.Matrix
import Domain.LinearAlgebra.LinearSystem
import Domain.LinearAlgebra.LinearExpr
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.MatrixRules -- for context
import Domain.LinearAlgebra.Vector
import Common.Context
import Common.Utils
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

parseSystem :: String -> (Context (LinearSystem SExprLin), [String])
parseSystem = f . parse p . scanWith s
 where
   s0 = newlinesAsSpecial scannerExpr
   s  = s0 {keywordOperators = "==" : keywordOperators s0 }
   p  = fmap inContext <$> pSystem
   f (Nothing, xs) = (inContext [], "System is not linear" : map show xs)
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

pEquations :: TokenParser a -> TokenParser (Equations a)
pEquations p = pLines True (pEquation p)


pEquation :: TokenParser a -> TokenParser (Equation a)
pEquation p = (:==:) <$> p <* pKey "==" <*> p
 
-----------------------------------------------------------
--- Parser

parseMatrix :: String -> (MatrixInContext SExpr, [String])
parseMatrix = f . parse p . scanWith s
 where
   s = newlinesAsSpecial scannerExpr
   p = (fmap (inContext . fmap simplifyExpr)) <$> pMatrix pFractional
   f (Nothing, xs) = (inContext (makeMatrix []), "Matrix is not rectangular" : map show xs)
   f (Just m, xs)  = (m, map show xs)

pMatrix :: TokenParser a -> TokenParser (Maybe (Matrix a))
pMatrix p = make <$> pLines True (pList1 p)
 where 
   make xs = if isRectangular xs then Just (makeMatrix xs) else Nothing 

parseVectors :: String -> (Context [Vector SExprGS], [Message Token])
parseVectors = parse p . scanWith s
 where
   s = newlinesAsSpecial scannerExpr
   p = (inContext . map (fmap simplifyExpr)) <$> pVectors pExpr

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

ppMatrixInContext :: Show a => MatrixInContext a -> String
ppMatrixInContext m = ppStringMatrix (ppFocus m) ++ "\n" ++ ppEnv m

ppEnv :: Show a => MatrixInContext a -> String
ppEnv m = "[" ++ commaList list ++ "]"
 where f s v = s ++ "=" ++ show (get v m)
       list  = [f "covered" covered, f "columnJ" columnJ]

ppFocus :: Show a => MatrixInContext a -> Matrix String
ppFocus c = mapWithPos f (matrix c)
 where f _ a
         {-  | focus c==p = "[" ++ show a ++ "]" -}
          | otherwise  = show a
     
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
              