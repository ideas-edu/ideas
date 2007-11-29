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
import Domain.Logic
import Domain.LinearAlgebra
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
runMatrix = runInterpreter matrixInterpreter

matrixInterpreter :: Interpreter (MatrixInContext Rational)
matrixInterpreter = Interpreter
   { parser        = error "no parser"
   , prettyPrinter = ppMatrixInContext
   , equivalence   = \x y -> applyD toReducedEchelon x == applyD toReducedEchelon y
   , finalProperty = inRowReducedEchelonForm . matrix
   , ruleset       = matrixRules
   , term          = Domain.LinearAlgebra.inContext defaultMatrix
   , strategy      = toReducedEchelon
   }

runLogic :: IO ()
runLogic = runInterpreter logicInterpreter

logicInterpreter :: Interpreter LogicInContext
logicInterpreter = Interpreter
   { parser        = Domain.Logic.inContext . fst . parseLogic
   , prettyPrinter = ppLogicInContext
   , equivalence   = \x y -> noContext x `eqLogic` noContext y
   , finalProperty = isDNF . noContext
   , ruleset       = map logicRuleInContext logicRules
   , strategy      = toDNF
   , term          = parser logicInterpreter "x/\\ (~y || ~(~z /\\ x))"
   }  