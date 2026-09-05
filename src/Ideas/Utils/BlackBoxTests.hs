-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Utils.BlackBoxTests 
   (blackBoxTests, TestRunner, Mode(..)) where

import Control.Monad
import Data.Char
import Data.IORef
import Data.List
import Ideas.Utils.TestSuite
import System.Directory
import System.IO
import qualified Data.Algorithm.Diff as Diff

type TestRunner = String -> IO String

data Mode = Interactive | Report | Replace

-- Returns the number of tests performed
blackBoxTests :: TestRunner -> Mode -> [String] -> String -> IO TestSuite
blackBoxTests runner mode exts path = do
   ref <- newIORef mode
   blackBoxTestsInMode ref runner exts path

blackBoxTestsInMode :: IORef Mode -> TestRunner -> [String] -> String -> IO TestSuite
blackBoxTestsInMode ref runner exts = rec
 where
    rec path = do
      -- analyse content
      xs0 <- getDirectoryContents path
      let (files, xs1) = partition (`elemExts` exts) xs0
          xs2          = map (path </>) (filter ((/= ".") . take 1) xs1)
      -- recursively visit subdirectories
      subs <- filterM doesDirectoryExist xs2
      rest <- mapM rec subs
      return $ suite ("Directory " ++ simplerDirectory path) $
         [ doBlackBoxTest ref runner (path </> x)
         | x <- files
         ] ++ rest

doBlackBoxTest :: IORef Mode -> TestRunner -> FilePath -> TestSuite
doBlackBoxTest ref runner path =
   assertMessageIO (stripDirectoryPart path) $
      -- Comparing output with expected output
      withFile path ReadMode $ \h1 -> do
         hSetBinaryMode h1 True
         txt <- hGetContents h1
         out <- runner txt
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
               return $ addPostHook (postHook out) $ message msg
 where
   expPath = baseOf path ++ ".exp"
   baseOf  = reverse . drop 1 . dropWhile (/= '.') . reverse

   postHook out status = do
      mode <- readIORef ref
      case mode of 
         _ | isOk status -> return ()
         Report -> return ()
         Replace -> replaceExpected
         Interactive  -> do
            putStrLn "Replace expected file: (y)es, (s)kip, (a)lways, or (n)ever?"
            putStr "? "
            answer <- getLine
            case map toLower answer of
               "y" -> replaceExpected
               "s" -> return () 
               "a" -> replaceExpected >> writeIORef ref Replace 
               "n" -> writeIORef ref Report
               _   -> postHook out status
    where
      replaceExpected = withFile expPath WriteMode $ \h ->
         hSetBinaryMode h True >> hPutStr h out

elemExts :: FilePath -> [String] -> Bool
elemExts s = any (\xs -> ('.':xs)  `isSuffixOf` s)

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