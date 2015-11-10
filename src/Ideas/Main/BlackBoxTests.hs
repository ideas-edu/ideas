-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Main.BlackBoxTests (blackBoxTests) where

import Control.Monad
import Data.Char
import Data.List
import Ideas.Common.Utils (useFixedStdGen, snd3)
import Ideas.Common.Utils.TestSuite
import Ideas.Encoding.ModeJSON
import Ideas.Encoding.ModeXML
import Ideas.Main.Logging
import Ideas.Service.DomainReasoner
import Ideas.Service.Request
import System.Directory
import System.IO
import qualified Data.Algorithm.Diff as Diff

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
                    JSON -> liftM snd3 (processJSON Nothing Nothing dr noLogRef txt)
                    XML  -> liftM snd3 (processXML  Nothing Nothing dr noLogRef txt)
         withFile expPath ReadMode $ \h2 -> do
            hSetBinaryMode h2 True
            expt <- hGetContents h2
            -- Force evaluation of the result, to make sure that
            -- all file handles are closed afterwards.
            let list1 = prepare expt
                list2 = prepare out
                msg   = unlines (path : diffs list1 list2)
            if list1 == list2 then return mempty else do
               force msg -- force evaluation of message before closing files
               return (message msg)
 where
   expPath = baseOf path ++ ".exp"
   baseOf  = reverse . drop 1 . dropWhile (/= '.') . reverse

force :: String -> IO ()
force s | sum (map ord s) >= 0 = return ()
        | otherwise = error "force"

prepare :: String -> [String]
prepare = filter (not . null) . lines . filter (/= '\r') . noVersion
 where
   noVersion s | "version\": \"" `isPrefixOf` s =
      "version\": \"X" ++ dropWhile (/='"') (drop 11 s)
   noVersion s | "version=\"" `isPrefixOf` s =
      "version=\"X" ++ dropWhile (/='"') (drop 9 s)
   noVersion (x:xs) = x:noVersion xs
   noVersion [] = []

diffs :: [String] -> [String] -> [String]
diffs xs ys = concatMap f $ Diff.getDiff xs ys
 where
   f (Diff.First a)  = ["- " ++ a]
   f (Diff.Second a) = ["+ " ++ a]
   f _ = []

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