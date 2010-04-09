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

import System.Directory
import Data.Maybe
import Common.Utils (safeHead, reportTest, useFixedStdGen, Some(..), snd3)
import Common.Exercise
import qualified Common.Strategy.Grammar as Grammar
import Control.Monad
import System.Environment
import Service.ExerciseList
import Service.Request

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
   forM_ exercises $ \(Some ex) ->
      checkExercise ex

   putStrLn "* 3. Unit tests"
   n <- unitTests
   putStrLn $ "** Number of unit tests: " ++ show n

-- Returns the number of tests performed
unitTests :: IO Int
unitTests = do
   args <- getArgs
   let dir = fromMaybe "test" (safeHead args)
   visit 0 dir
 where
   visit i path = do
      valid <- doesDirectoryExist path
      if not valid then return 0 else do
         -- analyse content
         xs <- getDirectoryContents path
         let xml  = filter (".xml"  `isSuffixOf`) xs
             json = filter (".json" `isSuffixOf`) xs
         putStrLn $ replicate (i+1) '*' ++ " " ++ simplerDirectory path
         -- perform tests
         forM json $ \x -> 
            performUnitTest JSON (path ++ "/" ++ x)
         forM xml $ \x -> 
            performUnitTest XML (path ++ "/" ++ x)
         -- recursively visit subdirectories
         is <- forM (filter ((/= ".") . take 1) xs) $ \x -> 
                  visit (i+1) (path ++ "/" ++ x)
         return (length (xml ++ json) + sum is)

performUnitTest :: DataFormat -> FilePath -> IO ()
performUnitTest format path = do
   useFixedStdGen -- fix the random number generator
   txt <- readFile path
   exp <- readFile expPath
   out <- case format of 
             JSON -> liftM snd3 (ModeJSON.processJSON txt)
             XML  -> liftM snd3 (ModeXML.processXML   txt)
   reportTest (stripDirectoryPart path) (out ~= exp)
 `catch` \_ -> 
    putStrLn $ "Error: testing " ++ path
 where
   expPath = baseOf path ++ ".exp"
   baseOf  = reverse . drop 1 . dropWhile (/= '.') . reverse
   x ~= y  = filterVersion x == filterVersion y
   
   filterVersion = 
      let p s = not (null s || "version" `isInfixOf` s)
      in unlines . filter p . lines

simplerDirectory :: String -> String
simplerDirectory s
   | "../"   `isPrefixOf` s = simplerDirectory (drop 3 s)
   | "test/" `isPrefixOf` s = simplerDirectory (drop 5 s)
   | otherwise = s

stripDirectoryPart :: String -> String
stripDirectoryPart = reverse . takeWhile (/= '/') . reverse

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
