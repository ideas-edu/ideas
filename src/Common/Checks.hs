-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Checks (main) where

import Common.Strategy
import Domain.Logic
import Domain.LinearAlgebra

main :: IO ()
main = do
   putStrLn "\n...checking strategy combinators"
   Common.Strategy.checks
   putStrLn "\n...checking logic domain"
   Domain.Logic.checks
   putStrLn "\n...checking matrix domain"
   Domain.LinearAlgebra.checks