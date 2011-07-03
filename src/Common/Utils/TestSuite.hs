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
module Common.Utils.TestSuite 
   ( -- * Test Suite Monad
     TestSuite, MonadIO(..)
     -- * Test suite constructors
   , suite, addProperty, addPropertyWith, warn
   , assertTrue, assertNull, assertEquals, assertIO
     -- * Running a test suite
   , runTestSuite, runTestSuiteResult
     -- * Test Suite Result
   , TestSuiteResult, subResults, findSubResult
   , messages, topMessages, numberOfTests
   , makeSummary, printSummary
     -- * Messages
   , Message, newMessage
   , isError, warning, messageLabel
   ) where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time
import Test.QuickCheck
import qualified Data.Foldable as F
import qualified Data.Sequence as S

----------------------------------------------------------------
-- Test Suite Monad

newtype TestSuiteM a = TSM { unTSM :: StateT Content IO a }

data Content = C 
   { column :: !Int -- Number of characters on the current line, for formatting
   , result :: !TestSuiteResult
   }

type TestSuite = TestSuiteM ()

instance Monad TestSuiteM where
   return  = TSM . return
   m >>= f = TSM (unTSM m >>= unTSM . f)
   fail s  = do assertTrue s False
                return (error "TestSuite.fail: do not bind result")

instance MonadIO TestSuiteM where
   liftIO =  TSM . liftIO

instance Monoid a => Monoid (TestSuiteM a) where
   mempty  = return mempty
   mappend = (>>)

----------------------------------------------------------------
-- Test suite constructors

-- | Construct a (named) test suite containing tests and other suites
suite :: String -> TestSuite -> TestSuite
suite s m = TSM $ do
   newline
   liftIO $ putStrLn s
   reset
   t <- updateDiffTime (withEmptyTree (unTSM m))
   addResult (suiteResult s t)

-- | Add a QuickCheck property to the test suite. The first argument is 
-- a label for the property
addProperty :: Testable prop => String -> prop -> TestSuite
addProperty = flip addPropertyWith stdArgs

-- | Add a QuickCheck property to the test suite, also providing a test
-- configuration (Args)
addPropertyWith :: Testable prop => String -> Args -> prop -> TestSuite
addPropertyWith s args p = TSM $ do
   newlineIndent
   r <- liftIO $ quickCheckWithResult args p
   reset
   let f = addResult . messageResult . setLabel s
   maybe (addResult okResult) f (toTestResult r)

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
addAssertion msg io = TSM $ do
   b <- liftIO (io `catch` \_ -> return False)
   if b then do 
      dot
      addResult okResult
    else do
      newlineIndent
      liftIO (print msg)
      reset
      addResult (messageResult msg)

withEmptyTree :: StateT Content IO () -> StateT Content IO TestSuiteResult
withEmptyTree m = do
   t0 <- gets result
   modify $ \c -> c {result = mempty}
   m
   tr <- gets result
   modify $ \c -> c {result = t0}
   return tr

-- formatting helpers
newline :: StateT Content IO ()
newline = do
   i <- gets column
   when (i>0) (liftIO $ putChar '\n')
   reset

newlineIndent :: StateT Content IO ()
newlineIndent = do
   newline
   liftIO $ putStr "   "
   modify $ \c -> c {column = 3}

dot :: StateT Content IO ()
dot = do
   i <- gets column
   unless (i>0 && i<60) newlineIndent
   liftIO $ putChar '.'
   modify $ \c -> c {column = column c+1}

addResult :: TestSuiteResult -> StateT Content IO ()
addResult r = modify $ \c -> c {result = result c `mappend` r}

reset :: StateT Content IO ()
reset = modify $ \c -> c {column = 0}

----------------------------------------------------------------
-- Running a test suite

runTestSuite :: TestSuite -> IO ()
runTestSuite s = runTestSuiteResult s >> return ()

runTestSuiteResult :: TestSuite -> IO TestSuiteResult
runTestSuiteResult s =
   updateDiffTime $ liftM result $
   execStateT (unTSM s >> newline) (C 0 mempty)

----------------------------------------------------------------
-- Test Suite Result

data TestSuiteResult = TSR 
   { messageSeq     :: S.Seq Message
   , suiteSeq       :: S.Seq (String, TestSuiteResult)
   , numberOfTests  :: !Int
   , diffTime       :: !NominalDiffTime
   }

instance Monoid TestSuiteResult where
   mempty = TSR mempty mempty 0 0
   mappend x y = TSR 
      { messageSeq    = messageSeq x `mappend` messageSeq y
      , suiteSeq      = suiteSeq x `mappend` suiteSeq y
      , numberOfTests = numberOfTests x + numberOfTests y
      , diffTime      = diffTime x + diffTime y
      }

okResult :: TestSuiteResult
okResult = mempty {numberOfTests = 1}

messageResult :: Message -> TestSuiteResult
messageResult m = okResult {messageSeq = S.singleton m}

suiteResult :: String -> TestSuiteResult -> TestSuiteResult
suiteResult s a = mempty 
   { suiteSeq = S.singleton (s, a)
   , numberOfTests = numberOfTests a
   }

-- one-line summary
instance Show TestSuiteResult where
   show res = 
      let (xs, ys) = partition isError (messages res)
      in "(tests: " ++ show (numberOfTests res) ++ 
         ", errors: " ++ show (length xs) ++
         ", warnings: " ++ show (length ys) ++ 
         ", " ++ show (diffTime res) ++ ")"

subResults :: TestSuiteResult -> [(String, TestSuiteResult)]
subResults = F.toList . suiteSeq

topMessages :: TestSuiteResult -> [Message]
topMessages = F.toList . messageSeq

messages :: TestSuiteResult -> [Message]
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

findSubResult :: String -> TestSuiteResult -> Maybe TestSuiteResult
findSubResult name = listToMaybe . recs
 where
   recs = concatMap rec . subResults
   rec (n, t) 
      | n == name = [t]
      | otherwise = recs t

printSummary :: TestSuiteResult -> IO ()
printSummary = putStrLn . makeSummary

makeSummary :: TestSuiteResult -> String
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
  
toTestResult :: Result -> Maybe Message
toTestResult res = 
   let make = Just . newMessage
   in case res of
         Success _ _ _           -> Nothing
         Failure _ _ _ _ msg _ _ -> make msg
         NoExpectedFailure _ _ _ -> make "no expected failure"
         GaveUp i _  _           -> fmap warning $ make $ 
                                    "passed only " ++ show i ++ " tests"

updateDiffTime :: MonadIO m => m TestSuiteResult -> m TestSuiteResult
updateDiffTime m = do
   (res, d) <- getDiffTime m
   return res {diffTime = d}

getDiffTime :: MonadIO m => m a -> m (a, NominalDiffTime)
getDiffTime action = do
   t0 <- liftIO getCurrentTime
   a  <- action
   t1 <- liftIO getCurrentTime
   return (a, diffUTCTime t1 t0)