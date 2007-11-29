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

import Strategy
import Logic
import Matrix

main :: IO ()
main = do
   putStrLn "\n...checking strategy combinators"
   Strategy.checks
   putStrLn "\n...checking logic domain"
   Logic.checks
   putStrLn "\n...checking matrix domain"
   Matrix.checks