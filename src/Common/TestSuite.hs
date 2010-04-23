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
-- A lightweight wrapper around the QuickCheck library. It introduces the
-- notion of a test suite, and it stores the test results for later inspection
-- (e.g., for the generation of a test report). A test suite has a monadic
-- interface.
--
-----------------------------------------------------------------------------
module Common.TestSuite 
   ( addProperty, suite, runTestSuite, TestSuite
   , assertTrue, assertNull, MonadIO(..), addPropertyWith
   , blackboxWith
   ) where

import Data.List
import Test.QuickCheck
import System.Random
import Common.Utils (commaList)
import Control.Monad.State

main :: IO ()
main = runTestSuite $ do
   suite "A" $ do
      addProperty "p1" p1
      suite "A1" $ addProperty "p2" p2
      addProperty "p3" p3
   suite "B" $ do
      addProperty "p4" p4
   suite "C" $ do
      addProperty "p5" p5
      assertTrue "sorted" (sort [3,2,1] == [1,2,3])
      assertEquals (sort [1,2,2]) (nub [1,2,2]) 
      
p1 xs = sort (xs::[Int]) == sort (sort xs)
p2 xs = reverse (reverse xs) == (xs::[Int])
p3 xs = head (sort xs) == minimum (xs::[Int])
p4 xs = sort (nub xs) == nub (sort (xs::[Int]))
p5 xs = reverse (sort xs) == sort (reverse (xs :: [Int]))

newtype TestSuiteM a = TSM { unTSM :: StateT TestState IO a }
 
type TestSuite = TestSuiteM ()
 
type TestState = TestTree Test

data TestTree a = Labeled String (TestTree a) 
                | TestTree a :+: TestTree a
                | Empty
                | Single a
 deriving Show

data Test = QuickCheck String Args Property
          | Assert String (IO Bool)
               
instance Monad TestSuiteM where
   return  = TSM . return
   m >>= f = TSM (unTSM m >>= unTSM . f)

instance MonadIO TestSuiteM where
   liftIO =  TSM . liftIO

suite :: String -> TestSuite -> TestSuite
suite s m = TSM $ do
   tree <- liftIO $ execStateT (unTSM m) Empty
   let new = Labeled s tree
   modify (:+: new)

addProperty :: Testable prop => String -> prop -> TestSuite
addProperty = flip addPropertyWith stdArgs

addPropertyWith :: Testable prop => String -> Args -> prop -> TestSuite
addPropertyWith s args p = TSM (modify (:+: new))
 where
   new = Single (QuickCheck s args (property p))

assertIO :: String -> IO Bool -> TestSuite
assertIO msg io = TSM $ do
   let new = Single (Assert msg io)
   modify (:+: new)

assertTrue :: String -> Bool -> TestSuite
assertTrue msg = assertIO msg . return

assertNull :: [String] -> TestSuite
assertNull xs =
   assertTrue (commaList xs) (null xs)

assertEquals :: (Eq a, Show a) => a -> a -> TestSuite
assertEquals x y = 
   let msg = "Not equal: " ++ show x ++ " and " ++ show y
   in assertTrue msg (x==y)

blackbox :: (String -> IO String) -> FilePath -> FilePath -> TestSuite
blackbox = blackboxWith (==)

blackboxWith :: (String -> String -> Bool) -> (String -> IO String) -> FilePath -> FilePath -> TestSuite
blackboxWith equalTo f fileInput fileExpected = 
   assertIO ("Unexpected output for " ++ show fileInput) $ do
      input    <- readFile fileInput
      expected <- readFile fileExpected
      output   <- f input
      return (output `equalTo` expected)
     `catch` \_ -> return False

type TestReport = TestTree Result

runTestSuite :: TestSuite -> IO ()
runTestSuite suite = do
   state <- execStateT (unTSM suite) Empty
   run state
   return ()
 where
   run :: TestState -> IO TestReport
   run state = 
      case state of
         Labeled s ts -> liftM (Labeled s) (putStrLn ("** " ++ s) >> run ts)
         t1 :+: t2    -> liftM2 (:+:) (run t1) (run t2)
         Empty        -> return Empty
         Single t     -> liftM Single (runTest t)
         
   runTest :: Test -> IO Result
   runTest test =
      case test of
         QuickCheck s args p -> do
            unless (null s) $ putStr (s ++ ": ")
            quickCheckWithResult args p
         Assert msg io -> do
            putChar '.'
            b <- io
            if b then return success else do
               putStrLn $ "Assertion failed: " ++ msg
               return (failure msg)

success :: Result
success = Success []

failure :: String -> Result
failure s = Failure (mkStdGen 0) 0 s []