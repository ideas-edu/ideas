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
import Common.Exercise
import Common.Grammar
import Common.Rewriting
import Common.Transformation

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
   putStrLn "\n...checking grammar combinators"
   Common.Grammar.checks
   LA.checks
   
   putStrLn "\n...checking exercises"
   checkExercise Logic.dnfExercise
   checkExercise LA.reduceMatrixExercise
   checkExercise LA.solveSystemExercise
   checkExercise LA.solveSystemWithMatrixExercise
   checkExercise LA.solveGramSchmidt
   checkExercise RA.cnfExercise
   checkExercise Fraction.simplExercise
   
   -- checking confluence
   logicConfluence
   
   -- unit-tests
   mathdoxRequests
   jsonRPCs
   xmlRequests

logicConfluence :: IO ()
logicConfluence = confluentFunction f rs
 where
   f    = normalizeWith ops . normalFormWith ops rs
   ops  = map makeCommutative Logic.logicOperators
   rwrs = Logic.logicRules \\ [Logic.ruleOrOverAnd, Logic.ruleCommOr, Logic.ruleCommAnd]
   rs   = [ r | RewriteRule r <- concatMap transformations rwrs ]
   -- eqs  = bothWays [ r | RewriteRule r <- concatMap transformations Logic.logicRules ]
   
mathdoxRequests :: IO ()
mathdoxRequests = do
   xs <- getDirectoryContents path
   let names = map f $ filter (".txt" `isSuffixOf`) xs
       f = reverse . drop 4 . reverse
   mapM_ oneRequest names
 where
   path = "../../test/mathdox-request"
   oneRequest base = do
      putStr $ take 40 (base ++ ".txt" ++ repeat ' ')
      txt <- readFile $ path ++ "/" ++ base ++ ".txt"
      exp <- readFile $ path ++ "/" ++ base ++ ".exp"
      let out = LAServer.respond (Just txt)
      putStrLn $ if filterVersion out == filterVersion exp then "ok" else "failed"

jsonRPCs :: IO ()
jsonRPCs = do
   xs <- getDirectoryContents path
   let names = map f $ filter (not . ("generate" `isPrefixOf`)) $ filter (".json" `isSuffixOf`) xs
       f = reverse . drop 5 . reverse
   mapM_ oneRequest names
 where
   path = "../../test/json-rpc"
   oneRequest base = do
      putStr $ take 40 (base ++ ".json" ++ repeat ' ')
      json     <- readFile $ path ++ "/" ++ base ++ ".json"
      exp      <- readFile $ path ++ "/" ++ base ++ ".exp"
      (out, _) <- ModeJSON.processJSON json
      putStrLn $ if filterVersion out == filterVersion exp then "ok" else "failed"

xmlRequests :: IO ()
xmlRequests = do
   xs <- getDirectoryContents path
   let names = map f $ filter (not . ("generate" `isPrefixOf`)) $ filter (".xml" `isSuffixOf`) xs
       f = reverse . drop 4 . reverse
   mapM_ oneRequest names
 where
   path = "../../test/xml-request"
   oneRequest base = do
      putStr $ take 40 (base ++ ".xml" ++ repeat ' ')
      xml      <- readFile $ path ++ "/" ++ base ++ ".xml"
      exp      <- readFile $ path ++ "/" ++ base ++ ".exp"
      (out, _) <- ModeXML.processXML Nothing xml
      putStrLn $ if filterVersion out == filterVersion exp then "ok" else "failed"

filterVersion :: String -> String
filterVersion = unlines . filter (not . null) . filter (not . ("version" `isInfixOf`)) . lines