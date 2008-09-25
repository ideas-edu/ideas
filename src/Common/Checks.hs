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

import Directory
import Common.Utils (reportTest, useFixedStdGen)
import Common.Exercise
import Common.Grammar
import Common.Rewriting
import Common.Transformation
import Control.Monad
import System.Environment

import qualified Domain.Logic as Logic
import qualified Domain.LinearAlgebra as LA
import qualified Domain.LinearAlgebra.Checks as LA
import qualified Domain.RelationAlgebra as RA
import qualified Domain.Fraction as Fraction

import qualified OpenMath.LAServer as LAServer
import qualified Service.ModeJSON as ModeJSON
import qualified Service.ModeXML as ModeXML
import Data.List

main :: IO ()
main = do
   putStrLn "I) Domain checks"
   Common.Grammar.checks
   LA.checks

   putStrLn "II) Exercise checks"
   checkExercise Logic.dnfExercise
   checkExercise LA.reduceMatrixExercise
   checkExercise LA.solveSystemExercise
   checkExercise LA.solveSystemWithMatrixExercise
   checkExercise LA.solveGramSchmidt
   checkExercise RA.cnfExercise
   checkExercise Fraction.simplExercise

   putStrLn "III) Confluence checks"
   logicConfluence
   
   putStrLn "IV) Unit tests"
   mathdoxRequests
   jsonRPCs
   xmlRequests

logicConfluence :: IO ()
logicConfluence = reportTest "logic rules" (isConfluent f rs)
 where
   f    = normalizeWith ops . normalFormWith ops rs
   ops  = map makeCommutative Logic.logicOperators
   rwrs = Logic.logicRules \\ [Logic.ruleOrOverAnd, Logic.ruleCommOr, Logic.ruleCommAnd]
   rs   = [ r | RewriteRule r <- concatMap transformations rwrs ]
   -- eqs  = bothWays [ r | RewriteRule r <- concatMap transformations Logic.logicRules ]
   
mathdoxRequests, jsonRPCs, xmlRequests :: IO ()
mathdoxRequests = testRequests (return . LAServer.respond . Just)       "mathdox-request" ".txt"
jsonRPCs        = testRequests (liftM fst . ModeJSON.processJSON)       "json-rpc"        ".json"
xmlRequests     = testRequests (liftM fst . ModeXML.processXML Nothing) "xml-request"     ".xml"

testRequests :: (String -> IO String) -> String -> String -> IO ()
testRequests eval subDir suffix = do
   path <- makePath subDir
   xs   <- getDirectoryContents path
   let names = map f $ filter (suffix `isSuffixOf`) xs
       f = reverse . drop (length suffix) . reverse
   flip mapM_ names $ \base -> do
      useFixedStdGen -- fix the random number generator
      txt <- readFile $ path ++ "/" ++ base ++ suffix
      exp <- readFile $ path ++ "/" ++ base ++ ".exp"
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