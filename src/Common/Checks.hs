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
-- (todo)
--
-----------------------------------------------------------------------------
module Main (main) where

import Common.Exercise
import Common.Grammar
import qualified Domain.Logic as Logic
import qualified Domain.Logic.Checks as Logic
import qualified Domain.LinearAlgebra as LA
import qualified Domain.LinearAlgebra.Checks as LA
import qualified Domain.Fraction as Frac
import qualified Domain.Fraction.Checks as Frac
import qualified Domain.RelationAlgebra as RA

main :: IO ()
main = do
   putStrLn "\n...checking grammar combinators"
   Common.Grammar.checks
   putStrLn "\n...checking logic domain"
   Logic.checks
   putStrLn "\n...checking linear algebra domain"
   LA.checks
   putStrLn "\n...checking fraction domain"
   Frac.checks
   putStrLn "\n...checking relation algebra domain"
   RA.checks
   putStrLn "\n...checking exercises"
   checkExercise Logic.dnfExercise
   checkExercise LA.reduceMatrixExercise
   checkExercise LA.solveSystemExercise
   checkExercise LA.solveSystemWithMatrixExercise
   checkExercise RA.cnfExercise
   checkExercise Frac.simplExercise