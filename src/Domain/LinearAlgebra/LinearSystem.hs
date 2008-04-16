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
module Domain.LinearAlgebra.LinearSystem where

import Domain.LinearAlgebra.Matrix (Matrix, makeMatrix, rows)
import Domain.LinearAlgebra.Equation
import Domain.LinearAlgebra.LinearExpr
import Data.List
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import Common.Utils
import Common.Unification

type LinearSystem a = Equations (LinearExpr a)

evalSystem :: Num a => (String -> a) -> LinearSystem a -> Bool
evalSystem = all . evalEquationWith . evalLinearExpr

invalidSystem :: Eq a => LinearSystem a -> Bool
invalidSystem = any invalidEquation

invalidEquation :: Eq a => Equation (LinearExpr a) -> Bool
invalidEquation eq@(lhs :==: rhs) = noVars eq && getConstant lhs /= getConstant rhs

getSolution :: Num a => LinearSystem a -> Maybe [(String, LinearExpr a)]
getSolution xs = do
   guard (distinct vars)
   guard (null (vars `intersect` frees))
   mapM make xs
 where
   vars  = concatMap (getVarsList . getLHS) xs
   frees = concatMap (getVarsList . getRHS) xs
   make (lhs :==: rhs) = do
      v <- isVar lhs
      return (v, rhs)
      
-- No constant on the left, no variables on the right
inStandardForm :: Num a => Equation (LinearExpr a) -> Bool
inStandardForm (lhs :==: rhs) = getConstant lhs == 0 && noVars rhs

toStandardForm :: Num a => Equation (LinearExpr a) -> Equation (LinearExpr a)
toStandardForm (lhs :==: rhs) =
      let c = toLinearExpr (getConstant rhs - getConstant lhs)
      in (lhs - rhs + c) :==: c


inSolvedForm :: Num a => LinearSystem a -> Bool
inSolvedForm xs = invalidSystem xs || isJust (getSolution xs)

homogeneous :: Num a => LinearSystem a -> Bool
homogeneous = all ((== Just 0) . isConstant . getRHS)

-- Conversions
systemToMatrix :: Num a => LinearSystem a -> (Matrix a, [String])
systemToMatrix system = (makeMatrix $ map (makeRow . toStandardForm) system, vars)
 where
   vars = getVarsList system
   makeRow (lhs :==: rhs) =
      map (`coefficientOf` lhs) vars ++ [getConstant rhs]

matrixToSystem :: Num a => Matrix a -> LinearSystem a
matrixToSystem = matrixToSystemWith variables

matrixToSystemWith :: Num a => [String] -> Matrix a -> LinearSystem a
matrixToSystemWith vs = map makeEquation . rows
 where
   varList = vs ++ (variables \\ vs)
   makeEquation [] = 0 :==: 0
   makeEquation xs = 
      let lhs = sum (zipWith (\v a -> toLinearExpr a * var v) varList (init xs))  
          rhs = toLinearExpr (last xs)
      in lhs :==: rhs
            
variables :: [String]
variables = map (\n -> 'x' : [n]) $ ['1' .. '9'] ++ ['a' .. 'z'] -- should be sorted!!