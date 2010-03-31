-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Main (main) where

import Directory
import Common.Utils (reportTest, useFixedStdGen, Some(..), snd3)
import Common.Exercise
import qualified Common.Strategy.Grammar as Grammar
import Control.Monad
import System.Environment
import Service.ExerciseList

import qualified Domain.LinearAlgebra.Checks as LA
import qualified Service.ModeJSON as ModeJSON
import qualified Service.ModeXML as ModeXML

import qualified Domain.Math.Numeric.Tests as MathNum
import qualified Domain.Math.Polynomial.Tests as MathPoly
import qualified Domain.Math.SquareRoot.Tests as MathSqrt
import qualified Domain.Math.Data.Interval as MathInterval

import qualified Text.UTF8 as UTF8
import qualified Text.JSON as JSON
import Data.List

main :: IO ()
main = do
   putStrLn "* 1. Domain checks"
   Grammar.checks
   MathNum.main
   MathPoly.tests
   MathSqrt.tests
   MathInterval.testMe
   LA.checks
   UTF8.testEncoding
   JSON.testMe

   putStrLn "* 2. Exercise checks"
   --checkExercise Logic.dnfExercise
   --checkExercise LA.gaussianElimExercise
   --checkExercise LA.linearSystemExercise
   --checkExercise LA.systemWithMatrixExercise
   --checkExercise LA.gramSchmidtExercise
   --checkExercise RA.cnfExercise
   forM_ exercises $ \(Some ex) ->
      checkExercise ex
   
   -- putStrLn "* 3. Confluence checks"
   -- logicConfluence
   
   putStrLn "* 3. Unit tests"
   mathdoxRequests
   jsonRPCs
   xmlRequests
   dwoDerivations

{-
logicConfluence :: IO ()
logicConfluence = reportTest "logic rules" (isConfluent f rs)
 where
   f    = normalizeWith ops . normalFormWith ops rs
   ops  = map makeCommutative Logic.logicOperators
   rwrs = Logic.logicRules \\ [Logic.ruleOrOverAnd, Logic.ruleCommOr, Logic.ruleCommAnd]
   rs   = [ r | RewriteRule r <- concatMap transformations rwrs ]
   -- eqs  = bothWays [ r | RewriteRule r <- concatMap transformations Logic.logicRules ]
-}
   
mathdoxRequests, jsonRPCs, xmlRequests, dwoDerivations :: IO ()
mathdoxRequests = testRequests (liftM snd3 . ModeXML.processXML)   "mathdox-request" ".txt"
jsonRPCs        = testRequests (liftM snd3 . ModeJSON.processJSON) "json-rpc"        ".json"
xmlRequests     = testRequests (liftM snd3 . ModeXML.processXML)   "xml-request"     ".xml"
dwoDerivations  = testRequests (liftM snd3 . ModeXML.processXML)   "dwo-derivations" ".xml"


testRequests :: (String -> IO String) -> String -> String -> IO ()
testRequests eval subDir suffix = do
   path <- makePath subDir
   xs   <- getDirectoryContents path
   let names = map f $ filter (suffix `isSuffixOf`) xs
       f = reverse . drop (length suffix) . reverse
   forM_ names $ \base -> do
      useFixedStdGen -- fix the random number generator
      txt <- readFile (path ++ "/" ++ base ++ suffix)
                `catch` \_ -> return ""
      exp <- readFile (path ++ "/" ++ base ++ ".exp")
                `catch` \_ -> return "" 
      out <- eval txt
      reportTest (base ++ suffix) (out ~= exp)
 where
   x ~= y = filterVersion x == filterVersion y
 
   filterVersion :: String -> String
   filterVersion = unlines . filter (not . null) . filter (not . ("version" `isInfixOf`)) . lines
   
   makePath :: String -> IO String
   makePath s = do
      args <- getArgs 
      case args of
         []  -> return $ "test/"  ++ s
         x:_ -> return $ x ++ "/" ++ s
