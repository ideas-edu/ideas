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
-- The XML specification comes with a test suite for testing the correctness
-- of a parser. This module performs these tests.
--
-----------------------------------------------------------------------------
module Main (main) where

import Text.XML.Interface
import Common.Utils (trim)
import Control.Monad.Error
import Data.List
import Data.Maybe

{-testje = do
   xs <- readFile "tmp.xml" >>= decoding
   print xs
   --print (take 3 $ drop 318 $ lines xs)
   print (parse document xs)
   print (map (\x -> (x, ord x)) xs) -}

rootDir :: String
rootDir  = "D:/xmlts20080827/xmlconf"

main :: IO ()
main = parseIO (rootDir ++ "/xmlconf.xml") >>= runTestSuite

printProfile :: Element -> IO ()
printProfile =
   maybe (return ()) putStrLn . findAttribute "PROFILE"

runTestSuite :: Element -> IO ()
runTestSuite e
   | name e /= "TESTSUITE" = fail "expected TESTSUITE"
   | otherwise = do
        printProfile e
        is <- mapM (runTestCases ".") (children e)
        putStrLn (replicate 40 '*')
        putStrLn $ "Test cases failed: " ++ show (sum is)

runTestCases :: String -> Element -> IO Int
runTestCases base e
   | name e /= "TESTCASES" = fail "expected TESTCASES"
   | otherwise = do
        printProfile e
        let newbase = fromMaybe base (findAttribute "xml:base" e)
        is <- forM (children e) $ \x -> 
           if name x == "TESTCASES" 
           then runTestCases newbase x 
           else do b <- runTest newbase x
                   return (if b then 0 else 1)
        return (sum is)

runTest :: String -> Element -> IO Bool
runTest base e
   | name e /= "TEST" = fail "expected TEST"
   | otherwise = do
        let filename = rootDir ++ "/" ++ base ++ "/" ++ uri
            uri      = fromMaybe "." (findAttribute "URI" e)
            testtype = fromMaybe ""  (findAttribute "TYPE" e)
            reccom   = findAttribute "RECOMMENDATION" e
        {-case reccom of 
           Nothing -> return ()
           Just "XML1.1" -> return ()
           Just "XML1.0-errata2e" -> return ()
           Just "NS1.0" -> return ()
           Just "NS1.1" -> return ()
           Just "XML1.0-errata3e" -> return ()
           Just "XML1.0-errata4e" -> return ()
           Just "NS1.0-errata1e" -> return () -}
        if reccom /= Nothing then return True else do
        putChar '.'
        mdoc <- (do a <- parseIO filename; return (Just a)) 
                   `catch` (\_ -> return Nothing)
        case mdoc of
           Just _
              --  not (accept document (show doc)) -> error ("pretty-print error: " ++ show doc)
              | testtype == "valid" -> return True
           Nothing 
              | testtype == "not-wf"  -> return True
              | testtype == "error"   -> return True
              | testtype == "invalid" -> return True
           _  | testtype /= "valid" {- && testtype /= "not-wf" -} -> return True
           _ -> do
              putStrLn $ "\nFilename: " ++ show filename
              putStrLn $ "Test type: " ++ show testtype
              putStrLn $ "Description: " ++ trim (getData e)
              return False   