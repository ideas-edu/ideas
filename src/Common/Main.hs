-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Main where

import Common.Strategy
import Common.Transformation
import Common.Interpreter
import Common.Assignment
import Domain.Logic
import Domain.LinearAlgebra
import Domain.LinearAlgebra.Checks (defaultMatrix)
import System.Environment
import Data.Char
                       
----------------------------------
-- Mini Interpreter

main :: IO ()
main = do
   args <- getArgs
   case map (map toLower) args of
      ["logic"]  -> runLogic
      ["matrix"] -> runMatrix
      _ -> do
         putStrLn $ unlines
            [ "Strategic Feedback Solver"
            , "   Usage: solver.exe (domain)"
            , ""
            , "Supported domains:"
            , "- logic  : bring a formula to dnf"
            , "- matrix : bring a matrix to reduced echelon form"
            ]


runMatrix :: IO ()
runMatrix = runInterpreter redEchelonAssignment

redEchelonAssignment :: Assignment (MatrixInContext Rational)
redEchelonAssignment = Assignment 
   { parser        = const $ Left (text "Sorry, no parser available", Nothing)
   , prettyPrinter = ppMatrixInContext
   , equivalence   = \x y -> applyD toReducedEchelon x == applyD toReducedEchelon y
   , equality      = (==)
   , finalProperty = inRowReducedEchelonForm . matrix
   , ruleset       = matrixRules
   , generator     = return $ Domain.LinearAlgebra.inContext defaultMatrix
   , strategy      = toReducedEchelon
   , configuration = defaultConfiguration
   }

runLogic :: IO ()
runLogic = runInterpreter dnfAssignment
   { prettyPrinter = ppLogicInContext
   }