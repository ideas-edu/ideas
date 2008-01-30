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

import Common.Assignment
import Common.Grammar
import qualified Domain.Logic as Logic
import qualified Domain.Logic.Checks as Logic
import qualified Domain.LinearAlgebra as LA
import qualified Domain.LinearAlgebra.Checks as LA
import qualified Domain.Fraction as Frac
import qualified Domain.Fraction.Checks as Frac

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
   putStrLn "\n...checking assignments"
   checkAssignment Logic.dnfAssignment
   checkAssignment LA.reduceMatrixAssignment