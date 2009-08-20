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

import Domain.Math.Data.Equation
import Domain.LinearAlgebra.Matrix (Matrix, makeMatrix, rows)
import Domain.LinearAlgebra.LinearView
import Data.List
import Data.Maybe
import Control.Monad
import Common.Utils
import Common.Uniplate

type LinearSystem a = Equations a

getVarsSystem :: IsLinear a => LinearSystem a -> [String]
getVarsSystem = foldr (\(lhs :==: rhs) xs -> getVars lhs `union` getVars rhs `union` xs) []

evalSystem :: (Uniplate a, IsLinear a) => (String -> a) -> LinearSystem a -> Bool
evalSystem = all . evalEquationWith . evalLinearExpr

invalidSystem :: IsLinear a => LinearSystem a -> Bool
invalidSystem = any invalidEquation

invalidEquation :: IsLinear a => Equation a -> Bool
invalidEquation (lhs :==: rhs) = null (getVars lhs ++ getVars rhs) && getConstant lhs /= getConstant rhs

getSolution :: IsLinear a => LinearSystem a -> Maybe [(String, a)]
getSolution xs = do
   guard (distinct vars)
   guard (null (vars `intersect` frees))
   mapM make xs
 where
   vars  = concatMap (getVars . getLHS) xs
   frees = concatMap (getVars . getRHS) xs
   make (lhs :==: rhs) = do
      v <- isVar lhs
      return (v, rhs)
      
-- No constant on the left, no variables on the right
inStandardForm :: IsLinear a => Equation a -> Bool
inStandardForm (lhs :==: rhs) = getConstant lhs == 0 && null (getVars rhs)

toStandardForm :: IsLinear a => Equation a -> Equation a
toStandardForm (lhs :==: rhs) =
      let c = getConstant rhs - getConstant lhs
      in (lhs - rhs + c) :==: c


inSolvedForm :: IsLinear a => LinearSystem a -> Bool
inSolvedForm xs = invalidSystem xs || isJust (getSolution xs)

homogeneous :: IsLinear a => LinearSystem a -> Bool
homogeneous = all ((== 0) . getRHS)

-- Conversions
systemToMatrix :: IsLinear a => LinearSystem a -> (Matrix a, [String])
systemToMatrix system = (makeMatrix $ map (makeRow . toStandardForm) system, vars)
 where
   vars = getVarsSystem system
   makeRow (lhs :==: rhs) =
      map (`coefficientOf` lhs) vars ++ [getConstant rhs]

matrixToSystem :: IsLinear a => Matrix a -> LinearSystem a
matrixToSystem = matrixToSystemWith variables

matrixToSystemWith :: IsLinear a => [String] -> Matrix a -> LinearSystem a
matrixToSystemWith vs = map makeEquation . rows
 where
   varList = vs ++ (variables \\ vs)
   makeEquation [] = 0 :==: 0
   makeEquation xs = 
      let lhs = sum (zipWith (\v a -> a * var v) varList (init xs))  
          rhs = last xs
      in lhs :==: rhs
            
variables :: [String]
variables = map (\n -> 'x' : [n]) $ ['1' .. '9'] ++ ['a' .. 'z'] -- should be sorted!!