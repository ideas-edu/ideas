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
module Domain.LinearAlgebra.LinearSystem where

import Domain.Math.Data.Relation
import Domain.LinearAlgebra.Matrix (Matrix, makeMatrix, rows)
import Domain.LinearAlgebra.LinearView
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Control.Monad
import Common.Utils
import Common.Utils.Uniplate
import Common.Rewriting
import qualified Data.Set as S

type LinearSystem a = Equations a

getVarsSystem :: IsLinear a => LinearSystem a -> [String]
getVarsSystem = S.toList . S.unions . map varSet . concatMap toList

evalSystem :: (Uniplate a, IsLinear a) => (String -> a) -> LinearSystem a -> Bool
evalSystem f = 
   let evalEq (x :==: y) = x==y
   in all (evalEq . fmap (evalLinearExpr f))

invalidSystem :: IsLinear a => LinearSystem a -> Bool
invalidSystem = any invalidEquation

invalidEquation :: IsLinear a => Equation a -> Bool
invalidEquation (lhs :==: rhs) = hasNoVar lhs && hasNoVar rhs && getConstant lhs /= getConstant rhs

getSolution :: IsLinear a => LinearSystem a -> Maybe [(String, a)]
getSolution xs = do
   guard (distinct vs)
   guard (null (vs `intersect` frees))
   mapM make xs
 where
   vs    = concatMap (vars . leftHandSide) xs
   frees = concatMap (vars . rightHandSide) xs
   make (lhs :==: rhs) = do
      v <- getVariable lhs
      return (v, rhs)
      
-- No constant on the left, no variables on the right
inStandardForm :: IsLinear a => Equation a -> Bool
inStandardForm (lhs :==: rhs) = getConstant lhs == 0 && hasNoVar rhs

toStandardForm :: IsLinear a => Equation a -> Equation a
toStandardForm (lhs :==: rhs) =
      let c = getConstant rhs - getConstant lhs
      in (lhs - rhs + c) :==: c


inSolvedForm :: IsLinear a => LinearSystem a -> Bool
inSolvedForm xs = invalidSystem xs || isJust (getSolution xs)

homogeneous :: IsLinear a => LinearSystem a -> Bool
homogeneous = all ((== 0) . rightHandSide)

-- Conversions
systemToMatrix :: IsLinear a => LinearSystem a -> (Matrix a, [String])
systemToMatrix system = (makeMatrix $ map (makeRow . toStandardForm) system, vs)
 where
   vs = getVarsSystem system
   makeRow (lhs :==: rhs) =
      map (`coefficientOf` lhs) vs ++ [getConstant rhs]

matrixToSystem :: IsLinear a => Matrix a -> LinearSystem a
matrixToSystem = matrixToSystemWith variables

matrixToSystemWith :: IsLinear a => [String] -> Matrix a -> LinearSystem a
matrixToSystemWith vs = map makeEquation . rows
 where
   varList = vs ++ (variables \\ vs)
   makeEquation [] = 0 :==: 0
   makeEquation xs = 
      let lhs = sum (zipWith (\v a -> a * variable v) varList (init xs))  
          rhs = last xs
      in lhs :==: rhs
            
variables :: [String]
variables = map (\n -> 'x' : [n]) $ ['1' .. '9'] ++ ['a' .. 'z'] -- should be sorted!!