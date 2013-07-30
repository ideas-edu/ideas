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
-- A lightweight wrapper around the QuickCheck library. It introduces the
-- notion of a test suite, and it stores the test results for later inspection
-- (e.g., for the generation of a test report). A test suite has a monadic
-- interface.
--
-----------------------------------------------------------------------------
module Ideas.Common.Utils.TestSuite
   ( -- * Test Suite Monad
     TestSuite, MonadIO(..)
     -- * Test suite constructors
   , suite, addProperty, addPropertyWith, warn
   , assertTrue, assertNull, assertEquals, assertIO
     -- * Running a test suite
   , runTestSuite, runTestSuiteResult
     -- * Test Suite Result
   , Result, subResults, findSubResult
   , messages, topMessages, numberOfTests
   , makeSummary, printSummary
     -- * Messages
   , Message, newMessage
   , isError, warning, messageLabel
   , module Data.Monoid
   ) where

import Control.Exception
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time
import Prelude hiding (catch)
import System.IO
import Test.QuickCheck hiding (Result)
import qualified Test.QuickCheck as QC
import qualified Data.Foldable as F
import qualified Data.Sequence as S

type Tests = [Test]

data Test = TestSuite String Tests
          | TestCase Message (IO Bool)
          | TestQC String Args Property

----------------------------------------------------------------
-- Test Suite Monad

newtype TestSuiteM a = TSM { unTSM :: Tests }

type TestSuite = TestSuiteM ()

instance Monad TestSuiteM where
   return _ = mempty
   TSM xs >>= f = TSM xs <> f (error "TestSuite.fail: do not bind result")

instance Monoid (TestSuiteM a) where
   mempty  = TSM []
   mappend (TSM xs) (TSM ys) = TSM (xs ++ ys)

----------------------------------------------------------------
-- Test suite constructors

-- | Construct a (named) test suite containing tests and other suites
suite :: String -> TestSuite -> TestSuite
suite s m = TSM [TestSuite s (unTSM m)]

-- | Add a QuickCheck property to the test suite. The first argument is
-- a label for the property
addProperty :: Testable prop => String -> prop -> TestSuite
addProperty = flip addPropertyWith stdArgs

-- | Add a QuickCheck property to the test suite, also providing a test
-- configuration (Args)
addPropertyWith :: Testable prop => String -> Args -> prop -> TestSuite
addPropertyWith s args p = TSM [TestQC s args (property p)]

assertTrue :: String -> Bool -> TestSuite
assertTrue msg = assertIO msg . return

assertNull :: Show a => String -> [a] -> TestSuite
assertNull s xs = addAssertion (f xs) (return (null xs))
 where f = setLabel s . newMessage . intercalate "\n" . map show

assertEquals :: (Eq a, Show a) => String -> a -> a -> TestSuite
assertEquals s x y = addAssertion (setLabel s msg) (return (x==y))
 where msg = newMessage ("Not equal: " ++ show x ++ " and " ++ show y)

assertIO :: String -> IO Bool -> TestSuite
assertIO s = addAssertion (setLabel s $ newMessage "Assertion failed")

warn :: String -> TestSuite
warn = (`addAssertion` return False) . warning . newMessage

-- local helpers
addAssertion :: Message -> IO Bool -> TestSuite
addAssertion msg io = TSM [TestCase msg io]

----------------------------------------------------------------
-- Running a test suite

runTestSuite :: TestSuite -> IO ()
runTestSuite = void . runTestSuiteResult

runTestSuiteResult :: TestSuite -> IO Result
runTestSuiteResult s = do
   hSetBuffering stdout NoBuffering
   updateDiffTime $ runWriteIO $ do
      a <- runTests (unTSM s)
      newline
      return a

runTests :: Tests -> WriteIO Result
runTests = liftM mconcat . mapM runTest

runTest :: Test -> WriteIO Result
runTest (TestSuite s xs)  = runSuite s xs
runTest (TestCase msg io) = runTestCase msg io
runTest (TestQC s args p) = runQC s args p

runSuite :: String -> Tests -> WriteIO Result
runSuite s m = do
   newline
   liftIO $ putStrLn s
   reset
   t <- updateDiffTime (runTests m)
   return (suiteResult s t)

runQC :: String -> Args -> Property -> WriteIO Result
runQC s args p = do
   newlineIndent
   r <- liftIO $ quickCheckWithResult args p
   reset
   let f = return . messageResult . setLabel s
   maybe (return okResult) f (toTestResult r)

runTestCase :: Message -> IO Bool -> WriteIO Result
runTestCase msg io = do
   b <- liftIO (io `catch` handler)
   if b then do
      dot
      return okResult
    else do
      newlineIndent
      liftIO (print msg)
      reset
      return (messageResult msg)
 where
   handler :: SomeException -> IO Bool
   handler _ = return False

-- formatting helpers
newtype WriteIO a = WriteIO { fromWriteIO :: StateT Int IO a }

runWriteIO :: WriteIO a -> IO a
runWriteIO = (`evalStateT` 0) . fromWriteIO

instance Monad WriteIO where
   return  = WriteIO . return
   fail    = WriteIO . fail
   m >>= f = WriteIO (fromWriteIO m >>= fromWriteIO . f)

instance MonadIO WriteIO where
   liftIO = WriteIO . liftIO

newline :: WriteIO ()
newline = do
   WriteIO $ do
      i <- get
      when (i>0) (liftIO $ putChar '\n')
   reset

newlineIndent :: WriteIO ()
newlineIndent = do
   newline
   WriteIO $ do
      liftIO $ putStr "   "
      put 3

dot :: WriteIO ()
dot = WriteIO $ do
   i <- get
   unless (i>0 && i<60) (fromWriteIO newlineIndent)
   liftIO $ putChar '.'
   modify succ
   
reset :: WriteIO ()
reset = WriteIO $ put 0

----------------------------------------------------------------
-- Test Suite Result

data Result = TSR
   { messageSeq     :: S.Seq Message
   , suiteSeq       :: S.Seq (String, Result)
   , numberOfTests  :: !Int
   , diffTime       :: !NominalDiffTime
   }

instance Monoid Result where
   mempty = TSR mempty mempty 0 0
   mappend x y = TSR
      { messageSeq    = messageSeq x `mappend` messageSeq y
      , suiteSeq      = suiteSeq x `mappend` suiteSeq y
      , numberOfTests = numberOfTests x + numberOfTests y
      , diffTime      = diffTime x + diffTime y
      }

okResult :: Result
okResult = mempty {numberOfTests = 1}

messageResult :: Message -> Result
messageResult m = okResult {messageSeq = S.singleton m}

suiteResult :: String -> Result -> Result
suiteResult s a = mempty
   { suiteSeq = S.singleton (s, a)
   , numberOfTests = numberOfTests a
   }

-- one-line summary
instance Show Result where
   show res =
      let (xs, ys) = partition isError (messages res)
      in "(tests: " ++ show (numberOfTests res) ++
         ", errors: " ++ show (length xs) ++
         ", warnings: " ++ show (length ys) ++
         ", " ++ show (diffTime res) ++ ")"

subResults :: Result -> [(String, Result)]
subResults = F.toList . suiteSeq

topMessages :: Result -> [Message]
topMessages = F.toList . messageSeq

messages :: Result -> [Message]
messages res =
   topMessages res ++ concatMap (messages . snd) (subResults res)

data Message = Message
   { message      :: String
   , isError      :: Bool
   , messageLabel :: Maybe String
   }

instance Show Message where
   show a = (if null pre then "" else pre ++ ": ") ++ message a
    where
       parens s = "(" ++ s ++ ")"
       pre = unwords $
                [ "Warning" | not (isError a) ] ++
                maybe [] (return . parens) (messageLabel a)

newMessage :: String -> Message
newMessage s = Message s True Nothing

warning :: Message -> Message
warning m = m {isError = False}

setLabel :: String -> Message -> Message
setLabel s m = m {messageLabel = Just s}

findSubResult :: String -> Result -> Maybe Result
findSubResult name = listToMaybe . recs
 where
   recs = concatMap rec . subResults
   rec (n, t)
      | n == name = [t]
      | otherwise = recs t

printSummary :: Result -> IO ()
printSummary = putStrLn . makeSummary

makeSummary :: Result -> String
makeSummary res = unlines $
   [ line
   , "Tests    : " ++ show (numberOfTests res)
   , "Failures : " ++ show (length xs)
   , "Warnings : " ++ show (length ys)
   , "\nTime     : " ++ show (diffTime res)
   , "\nSuites: "
   ] ++ map f (subResults res)
     ++ [line]
 where
   line        = replicate 75 '-'
   (xs, ys)    = partition isError (messages res)
   f (name, r) = "   " ++ name ++ "   " ++ show r

-----------------------------------------------------
-- Utility functions

toTestResult :: QC.Result -> Maybe Message
toTestResult res =
   let make = Just . newMessage
   in case res of
         Success {}             -> Nothing
         Failure {reason = msg} -> make msg
         NoExpectedFailure {}   -> make "no expected failure"
         GaveUp {numTests = i}  -> fmap warning $ make $
                                   "passed only " ++ show i ++ " tests"

updateDiffTime :: MonadIO m => m Result -> m Result
updateDiffTime m = do
   (res, d) <- getDiffTime m
   return res {diffTime = d}

getDiffTime :: MonadIO m => m a -> m (a, NominalDiffTime)
getDiffTime action = do
   t0 <- liftIO getCurrentTime
   a  <- action
   t1 <- liftIO getCurrentTime
   return (a, diffUTCTime t1 t0)