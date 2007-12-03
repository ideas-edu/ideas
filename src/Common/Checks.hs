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
import Domain.Logic
import Domain.LinearAlgebra

main :: IO ()
main = do
   putStrLn "\n...checking regular expression combinators"
   Common.RegExp.checks
   putStrLn "\n...checking logic domain"
   Domain.Logic.checks
   putStrLn "\n...checking matrix domain"
   Domain.LinearAlgebra.checks