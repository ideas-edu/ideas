-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  unknown
--
-- A commandline tool to assess student solutions.
-----------------------------------------------------------------------------

module Main where

import Domain.Programming.Strategies
import Domain.Programming.Strategies99
import Domain.Programming.Helium
import Domain.Programming.Prog
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then printUsage
    else do
      check (head args) (args !! 1)

check :: String -> String -> IO ()
check modelFile studentFile = do 
  _       <- readFile modelFile
  student <- readFile studentFile
  either putStrLn p $ compile student
    where p = print . isSolution ["fromBin"] fromBinStrategy
  
printUsage :: IO ()
printUsage = putStrLn "Usage: assess <model file> <student file>"
   