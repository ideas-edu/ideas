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
module Documentation.SelfCheck (selfCheck) where

import System.Directory
import Common.TestSuite
import Common.Utils (useFixedStdGen, Some(..), snd3)
import Common.Exercise
import Service.ExercisePackage
import qualified Common.Strategy.Grammar as Grammar
import Control.Monad
import Service.Request
import Service.DomainReasoner
import Service.ModeJSON
import Service.ModeXML
import qualified Text.UTF8 as UTF8
import qualified Text.JSON as JSON
import Data.List
import System.Time

selfCheck :: String -> DomainReasoner TestSuite
selfCheck dir = do
   pkgs          <- getPackages
   domainSuite   <- getTestSuite
   blackboxSuite <- blackBoxTests dir
   
   return $ do
      suite "Framework checks" $ do
         Grammar.testMe
         suite "Text encodings" $ do
            addProperty "UTF8 encoding" UTF8.propEncoding
            addProperty "JSON encoding" JSON.propEncoding

      suite "Domain checks" domainSuite

      suite "Exercise checks" $
         forM_ pkgs $ \(Some pkg) ->
            exerciseTestSuite (exercise pkg)
      
      suite "Black box tests" blackboxSuite

-- Returns the number of tests performed
blackBoxTests :: String -> DomainReasoner TestSuite
blackBoxTests path = do
   valid <- liftIO $ doesDirectoryExist path
   if not valid then return (return ()) else do
      -- analyse content
      xs <- liftIO $ getDirectoryContents path
      let xml  = filter (".xml"  `isSuffixOf`) xs
          json = filter (".json" `isSuffixOf`) xs
      -- perform tests
      ts1 <- forM json $ \x -> 
                doBlackBoxTest JSON (path ++ "/" ++ x)
      ts2 <- forM xml $ \x -> 
                doBlackBoxTest XML (path ++ "/" ++ x)
      -- recursively visit subdirectories
      ts3 <- forM (filter ((/= ".") . take 1) xs) $ \x -> 
                blackBoxTests (path ++ "/" ++ x)
      return $ suite (simplerDirectory path) $ 
         sequence_ (ts1 ++ ts2 ++ ts3)

doBlackBoxTest :: DataFormat -> FilePath -> DomainReasoner TestSuite
doBlackBoxTest format path = do
   liftIO useFixedStdGen -- fix the random number generator
   txt <- liftIO $ readFile path
   exp <- liftIO $ readFile expPath
   out <- case format of 
             JSON -> liftM snd3 (processJSON txt)
             XML  -> liftM snd3 (processXML txt) 
           `catchError` 
             \_ -> return "Error"
   let name = stripDirectoryPart path
   -- Silly code, to force evaluation of boolean
   if out ~= exp 
      then return (assertTrue name True)
      else return (assertTrue name False)
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
showDiffWith :: MonadIO m => (TimeDiff -> IO ()) -> m a -> m a
showDiffWith f action = do
   t0 <- liftIO getClockTime
   a  <- action
   t1 <- liftIO getClockTime
   liftIO (f (diffClockTimes t1 t0))
   return a

totalDiff :: MonadIO m => m a -> m a
totalDiff = showDiffWith (putStrLn . ("*** Total time: "++) . formatDiff)
   
timeDiff :: MonadIO m => m a -> m a
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