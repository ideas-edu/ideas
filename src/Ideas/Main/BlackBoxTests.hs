-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Main.BlackBoxTests (blackBoxTests) where

import Control.Monad
import Control.Monad.Error
import Data.List
import Ideas.Common.Utils (useFixedStdGen, snd3)
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON
import Ideas.Encoding.ModeXML
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import System.Directory
import System.IO hiding (readFile)
import Data.Char

-- Returns the number of tests performed
blackBoxTests :: DomainReasoner -> String -> TestSuite
blackBoxTests dr path = do
   -- analyse content
   xs0 <- liftIO (getDirectoryContents path)
   let (xml,  xs1) = partition (".xml"  `isSuffixOf`) xs0
       (json, xs2) = partition (".json" `isSuffixOf`) xs1
   -- perform tests
   forM_ json $ \x ->
      doBlackBoxTest dr JSON (path ++ "/" ++ x)
   forM_ xml $ \x ->
      doBlackBoxTest dr XML (path ++ "/" ++ x)
   -- recursively visit subdirectories
   forM_ (filter ((/= ".") . take 1) xs2) $ \x -> do
      let p = path ++ "/" ++ x
      valid <- liftIO (doesDirectoryExist p)
      when valid $
         suite ("Directory " ++ simplerDirectory p) 
               (blackBoxTests dr p)

doBlackBoxTest :: DomainReasoner -> DataFormat -> FilePath -> TestSuite
doBlackBoxTest dr format path =
   assertIO (stripDirectoryPart path) $ do
      -- Comparing output with expected output
      useFixedStdGen -- fix the random number generator
      withFile path ReadMode $ \h1 -> do
         txt <- hGetContents h1
         out  <- case format of
                    JSON -> liftM snd3 (processJSON False dr txt)
                    XML  -> liftM snd3 (processXML dr Nothing txt)
         withFile expPath ReadMode $ \h2 -> do
            expt <- hGetContents h2
            -- Force evaluation of the result, to make sure that
            -- all file handles are closed afterwards.
            let result = out ~= expt
            result `seq` return result
 where
   expPath = baseOf path ++ ".exp"
   baseOf  = reverse . drop 1 . dropWhile (/= '.') . reverse
   x ~= y  = filterVersion x == filterVersion y -- compare line-based

filterVersion :: String -> [String]
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