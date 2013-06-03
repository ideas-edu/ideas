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
import System.IO

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
                    JSON -> liftM snd3 (processJSON False dr txt)
                    XML  -> liftM snd3 (processXML dr Nothing txt)
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