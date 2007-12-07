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

import Common.RegExp
import qualified Domain.Logic.Checks as Logic
import qualified Domain.LinearAlgebra.Checks as LA

main :: IO ()
main = do
   putStrLn "\n...checking regular expression combinators"
   Common.RegExp.checks
   putStrLn "\n...checking logic domain"
   Logic.checks
   putStrLn "\n...checking linear algebra domain"
   LA.checks