-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.SelfCheck (performSelfCheck) where

import System.Directory
import Common.Utils (reportTest, useFixedStdGen, Some(..), snd3)
import Common.Exercise
import Service.ExercisePackage
import qualified Common.Strategy.Grammar as Grammar
import Control.Monad
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
import System.Time

performSelfCheck :: String -> [Some ExercisePackage] -> IO ()
performSelfCheck dir list = totalDiff $ do
   timeDiff $ do
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
   forM_ list $ \(Some pkg) ->
      timeDiff $ checkExercise (exercise pkg)

   timeDiff $ do
      putStrLn "* 3. Unit tests"
      n <- unitTests list dir
      putStrLn $ "** Number of unit tests: " ++ show n
   
-- Returns the number of tests performed
unitTests :: [Some ExercisePackage] -> String -> IO Int
unitTests list = visit 0
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
            performUnitTest list JSON (path ++ "/" ++ x)
         forM xml $ \x -> 
            performUnitTest list XML (path ++ "/" ++ x)
         -- recursively visit subdirectories
         is <- forM (filter ((/= ".") . take 1) xs) $ \x -> 
                  visit (i+1) (path ++ "/" ++ x)
         return (length (xml ++ json) + sum is)

performUnitTest :: [Some ExercisePackage] -> DataFormat -> FilePath -> IO ()
performUnitTest list format path = do
   useFixedStdGen -- fix the random number generator
   txt <- readFile path
   exp <- readFile expPath
   out <- case format of 
             JSON -> liftM snd3 (ModeJSON.processJSON list (Just "self-check") txt)
             XML  -> liftM snd3 (ModeXML.processXML   list (Just "self-check") txt)
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

-- Helper functions
showDiffWith :: (TimeDiff -> IO ()) -> IO a -> IO a
showDiffWith f action = do
   t0 <- getClockTime
   a  <- action
   t1 <- getClockTime
   f (diffClockTimes t1 t0)
   return a

totalDiff :: IO a -> IO a
totalDiff = showDiffWith (putStrLn . ("*** Total time: "++) . formatDiff)
   
timeDiff :: IO a -> IO a
timeDiff = showDiffWith (putStrLn . ("+++ Time: "++) . formatDiff) 

formatDiff :: TimeDiff -> String
formatDiff d@(TimeDiff z1 z2 z3 h m s p)
   | any (/=0) [z1,z2,z3] = timeDiffToString d
   | s >= 60      = formatDiff (timeDiff ((h*60+m)*60+s) p)
   | h==0 && m==0 = show inSec ++ " secs"
   | otherwise    = show (60*h+m) ++ ":" ++ digSec ++ " mins" 
 where
   milSec = 1000*toInteger s + p `div` 1000000000
   inSec  = fromIntegral milSec / 1000
   digSec = (if s < 10 then ('0' :) else id) (show s)
   timeDiff n p = 
      let (rest, s) = n `divMod` 60
          (h, m)    = rest `divMod` 60
      in TimeDiff 0 0 0 h m s p