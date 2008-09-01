{-# OPTIONS -XExistentialQuantification #-}
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
module OpenMath.StrategyTable where

import Common.Context
import Common.Exercise
import Common.Utils (Some(..))
import Domain.LinearAlgebra (reduceMatrixExercise, solveSystemExercise, solveGramSchmidt, solveSystemWithMatrixExercise, LinearSystem)
import Domain.LinearAlgebra (makeMatrix, var)
import Domain.LinearAlgebra.Equation (Equation)
import Domain.LinearAlgebra.Vector (fromList)
import Domain.Math.SExpr
import qualified Domain.LinearAlgebra.Equation as LA
import OpenMath.Conversion
import OpenMath.Object

type StrategyID = String

versionNr :: String
versionNr = "0.4.0"

oneliner :: String -> String
oneliner = unwords . concatMap words . lines

defaultURL :: Bool -> String
defaultURL b = "http://ideas.cs.uu.nl/cgi-bin/service.cgi?" ++ (if b then "mode=html&" else "") ++ "input="

data ExprExercise a = IsOMOBJ a => ExprExercise (Exercise (Context a))

data StrategyEntry = Entry 
   { strategyNr   :: String
   , exprExercise :: Some ExprExercise
   , functions    :: [String]
   , examples     :: [OMOBJ]
   }
 
entry :: IsOMOBJ a => String -> Exercise (Context a) -> [String] -> [a] -> StrategyEntry
entry nr a fs ex = Entry nr (Some (ExprExercise a)) fs (map toOMOBJ ex)

strategyTable :: [StrategyEntry]
strategyTable =
   [ entry "2.5" reduceMatrixExercise
        ["toReducedEchelon"]
        [makeMatrix [[6, 3], [2, 4]], makeMatrix [[0,1,1,1], [1,2,3,2], [3,1,1,3]]]
   , entry "1.7" solveSystemExercise
        ["generalSolutionLinearSystem", "systemToEchelonWithEEO", "backSubstitutionSimple"]
        [sys1, sys2, sys3]
   , entry "2.6" solveSystemWithMatrixExercise
        ["generalSolutionSystemWithMatrix"]
        (map Left [sys1, sys2, sys3])
   , entry "8.6" solveGramSchmidt       
        ["gramSchmidt"]
        [[fromList [1,1,1,1], fromList [3,3,1,1], fromList [7,9,3,5]]]
   ]
 where
   x1, x2, x3, x4 :: Simplification a => SExprF a
   x1 = var "x1"
   x2 = var "x2"
   x3 = var "x3"
   x4 = var "x4"
   -- (x1, x2, x3, x4) = (var "x1", var "x2", var "x3", var "x4")
   sys1, sys2, sys3 :: Simplification a => LinearSystem (SExprF a)
   sys1 = [x2 + 2 * x3 LA.:==: 1, x1 + 2 * x2 + 3 * x3 LA.:==: 2, 3 * x1 + x2 + x3 LA.:==: 3]
   sys2 = [x1 + 2 * x2 + 3 * x3 - x4 LA.:==: 0, 2 * x1 + 3 * x2 - x3 + 3 * x4 LA.:==: 0, 4 * x1 + 6 * x2 + x3 + 2 * x4 LA.:==: 0 ]
   sys3 = [ x1 + x2 - 2*x3 LA.:==: 0, 2*x1 + x2 - 3*x3 LA.:==: 0, 4*x1 - 2*x2 - 2*x3 LA.:==: 0, 6*x1 - x2 - 5*x3 LA.:==: 0, 7*x1 - 3*x2 - 4*x3 LA.:==: 1]