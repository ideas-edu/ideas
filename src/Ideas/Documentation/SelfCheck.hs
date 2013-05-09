-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Documentation.SelfCheck (selfCheck, blackBoxTests) where

import Ideas.Common.Exercise
import Ideas.Common.Utils (useFixedStdGen, Some(..), snd3)
import Ideas.Common.Utils.TestSuite
import Control.Monad
import Control.Monad.Error
import Data.List
import Ideas.Service.DomainReasoner
import Ideas.Service.ModeJSON
import Ideas.Service.ModeXML
import Ideas.Service.Request
import System.Directory
import System.IO
import qualified Ideas.Common.Algebra.Boolean as Algebra
import qualified Ideas.Common.Algebra.Field as Algebra
import qualified Ideas.Common.Rewriting.Substitution as Substitution
import qualified Ideas.Common.Rewriting.Unification as Unification
import qualified Ideas.Common.Traversal.Tests as Traversal
import qualified Ideas.Common.Strategy.Tests as Strategy
import qualified Ideas.Text.JSON as JSON
import qualified Ideas.Text.OpenMath.Tests as OpenMath
import qualified Ideas.Text.UTF8 as UTF8

selfCheck :: DomainReasoner -> String -> TestSuite
selfCheck dr dir = do
   suite "Framework checks" $ do
      suite "Text encodings" $ do
         addProperty "UTF8 encoding" UTF8.propEncoding
         addProperty "JSON encoding" JSON.propEncoding
         addProperty "OpenMath encoding" OpenMath.propEncoding
      Substitution.tests
      Unification.unificationTests
      Traversal.tests
      Strategy.tests
      suite "Field properties" $
         mapM_ (addProperty "field") Algebra.propsField
      suite "Boolean properties" $
         mapM_ (addProperty "boolean") Algebra.propsBoolean

   suite "Domain checks" (testSuite dr)

   suite "Exercise checks" $
      forM_ (exercises dr) $ \(Some ex) ->
         exerciseTestSuite ex

   suite "Black box tests" $
      join (liftIO (blackBoxTests dr dir))

-- Returns the number of tests performed
blackBoxTests :: DomainReasoner -> String -> IO TestSuite
blackBoxTests dr path = do
   putStrLn ("Scanning " ++ path)
   -- analyse content
   xs0 <- getDirectoryContents path
   let (xml,  xs1) = partition (".xml"  `isSuffixOf`) xs0
       (json, xs2) = partition (".json" `isSuffixOf`) xs1
   -- perform tests
   ts1 <- forM json $ \x ->
             doBlackBoxTest dr JSON (path ++ "/" ++ x)
   ts2 <- forM xml $ \x ->
             doBlackBoxTest dr XML (path ++ "/" ++ x)
   -- recursively visit subdirectories
   ts3 <- forM (filter ((/= ".") . take 1) xs2) $ \x -> do
             let p = path ++ "/" ++ x
             valid <- doesDirectoryExist p
             if not valid
                then return (return ())
                else liftM (suite $ "Directory " ++ simplerDirectory p)
                           (blackBoxTests dr p)
   return $
      sequence_ (ts1 ++ ts2 ++ ts3)

doBlackBoxTest :: DomainReasoner -> DataFormat -> FilePath -> IO TestSuite
doBlackBoxTest dr format path = do
   hSetBinaryMode stdout True
   b <- doesFileExist expPath
   return $ if not b
      then warn $ expPath ++ " does not exist"
      else assertIO (stripDirectoryPart path) $ do
         -- Comparing output with expected output
         (h1, h2, txt, expt) <- liftIO $ do
            useFixedStdGen -- fix the random number generator
            h1   <- openBinaryFile path ReadMode
            txt  <- hGetContents h1
            h2   <- openBinaryFile expPath ReadMode
            expt <- hGetContents h2
            return (h1, h2, txt, expt)
         out  <- case format of
                    JSON -> liftM snd3 (processJSON dr txt)
                    XML  -> liftM snd3 (processXML dr txt)
         -- Conditional forces evaluation of the result, to make sure that
         -- all file handles are closed afterwards.
         if out ~= expt 
            then liftIO (hClose h1 >> hClose h2) >> return True 
            else liftIO (hClose h1 >> hClose h2) >> return False
       `catchError`
         \_ -> return False
 where
   expPath = baseOf path ++ ".exp"
   baseOf  = reverse . drop 1 . dropWhile (/= '.') . reverse
   x ~= y  = filterVersion x == filterVersion y -- compare line-based

   filterVersion =
      let p s = not (null s || "version" `isInfixOf` s)
      in filter p . lines . filter (/= '\r')

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