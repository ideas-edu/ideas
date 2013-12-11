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

import Control.Exception
import Control.Monad
import Data.List
import Ideas.Common.Utils (useFixedStdGen, snd3)
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON
import Ideas.Encoding.ModeXML
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import System.Directory
import System.IO hiding (readFile)

-- Returns the number of tests performed
blackBoxTests :: DomainReasoner -> String -> IO TestSuite
blackBoxTests dr path = do
   -- analyse content
   xs0 <- getDirectoryContents path
   let (xml,  xs1) = partition (".xml"  `isSuffixOf`) xs0
       (json, xs2) = partition (".json" `isSuffixOf`) xs1
       xs3         = map (path </>) (filter ((/= ".") . take 1) xs2)
   -- recursively visit subdirectories
   subs <- filterM doesDirectoryExist xs3
   rest <- mapM (blackBoxTests dr) subs
   return $ suite ("Directory " ++ simplerDirectory path) $
      [ doBlackBoxTest dr JSON (path </> x)
      | x <- json
      ] ++
      [ doBlackBoxTest dr XML (path </> x)
      | x <- xml
      ] ++
      rest

doBlackBoxTest :: DomainReasoner -> DataFormat -> FilePath -> TestSuite
doBlackBoxTest dr format path =
   assertMessageIO (stripDirectoryPart path) $ do
      -- Comparing output with expected output
      useFixedStdGen -- fix the random number generator
      withFile path ReadMode $ \h1 -> do
         hSetBinaryMode h1 True
         txt <- hGetContents h1
         out  <- case format of
                    JSON -> liftM snd3 (processJSON Nothing False dr txt)
                    XML  -> liftM snd3 (processXML Nothing dr Nothing txt)
         withFile expPath ReadMode $ \h2 -> do
            hSetBinaryMode h2 True
            expt <- hGetContents h2
            -- Force evaluation of the result, to make sure that
            -- all file handles are closed afterwards.
            if out ~= expt then return mempty else return (message path)
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

(</>) :: FilePath -> FilePath -> FilePath
x </> y = x ++ "/" ++ y

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