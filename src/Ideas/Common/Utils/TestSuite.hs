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
-- A lightweight wrapper for organizing tests (including QuickCheck tests). It 
-- introduces the notion of a test suite, and it stores the test results for 
-- later inspection (e.g., for the generation of a test report). A TestSuite 
-- is a monoid.
--
-----------------------------------------------------------------------------
module Ideas.Common.Utils.TestSuite
   ( -- * TestSuite
     TestSuite, module Data.Monoid
   , suite, useProperty, usePropertyWith
   , assertTrue, assertNull, assertEquals, assertIO
   , assertMessage, onlyWarnings, rateOnError
     -- * Running a test suite
   , runTestSuite, runTestSuiteResult
     -- * Test Suite Result
   , Result, subResults, findSubResult
   , justOneSuite, allMessages, topMessages
   , nrOfTests, nrOfErrors, nrOfWarnings
   , timeInterval, makeSummary, printSummary
     -- * Message
   , Message, message, warning, messageLines
     -- * Status
   , Status, HasStatus(..)
   , isError, isWarning, isOk
     -- * Rating
   , Rating, HasRating(..)
   ) where

import Control.Exception
import Control.Monad.State.Strict
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Time
import Prelude hiding (catch)
import System.IO 
import Test.QuickCheck hiding (Result)
import qualified Data.Sequence as S

----------------------------------------------------------------
-- Test Suite

newtype TestSuite = TS (S.Seq Test)

data Test = Case  String (IO Message)
          | Suite String TestSuite

instance Monoid TestSuite where
   mempty = TS mempty
   TS xs `mappend` TS ys = TS (xs <> ys)

tests :: TestSuite -> [Test]
tests (TS xs) = toList xs

makeTestSuite :: Test -> TestSuite
makeTestSuite = TS . S.singleton

----------------------------------------------------------------
-- Test suite constructors

-- | Construct a (named) test suite containing test cases and other suites
suite :: String -> [TestSuite] -> TestSuite
suite s = makeTestSuite . Suite s . mconcat

-- | Turn a QuickCheck property into the test suite. The first argument is
-- a label for the property
useProperty :: Testable prop => String -> prop -> TestSuite
useProperty = flip usePropertyWith stdArgs

-- | Turn a QuickCheck property into the test suite, also providing a test
-- configuration (Args)
usePropertyWith :: Testable prop => String -> Args -> prop -> TestSuite
usePropertyWith s args = 
   makeTestSuite . Case s . liftM make . quickCheckWithResult args {chatty=False}
 where
   make qc =
      case qc of
         Success {} -> 
            mempty
         Failure {reason = msg} -> 
            message msg
         NoExpectedFailure {} -> 
            message "no expected failure"
         GaveUp {numTests = i} -> 
            warning ("passed only " ++ show i ++ " tests")

assertTrue :: String -> Bool -> TestSuite
assertTrue s = assertIO s . return

assertNull :: Show a => String -> [a] -> TestSuite
assertNull s xs = assertMessages s (null xs) (map show xs)

assertEquals :: (Eq a, Show a) => String -> a -> a -> TestSuite
assertEquals s x y = assertMessage s (x==y) $ 
   "not equal " ++ show x ++ " and " ++ show y

assertMessage :: String -> Bool -> String -> TestSuite
assertMessage s b = assertMessages s b . return

assertMessages :: String -> Bool -> [String] -> TestSuite
assertMessages s b xs = makeTestSuite . Case s $ return $ 
   if b then mempty else mconcat (map message xs)

assertIO :: String -> IO Bool -> TestSuite
assertIO s = makeTestSuite . Case s . liftM f
 where
   f b = if b then mempty else message "assertion failed"

-- | All errors are turned into warnings
onlyWarnings :: TestSuite -> TestSuite
onlyWarnings = changeMessages $ \m -> 
   m { messageStatus = messageStatus m  `min` Warning
     , messageRating = mempty
     }

rateOnError :: Int -> TestSuite -> TestSuite
rateOnError n = changeMessages $ \m -> 
   if isError m then m { messageRating = Rating n } else m

changeMessages :: (Message -> Message) -> TestSuite -> TestSuite
changeMessages f = changeTS
 where
   changeTS   (TS xs)     = TS (fmap changeTest xs)
   changeTest (Case s io) = Case s (liftM f io)
   changeTest (Suite s t) = Suite s (changeTS t)

----------------------------------------------------------------
-- Running a test suite

runTestSuite :: Bool -> TestSuite -> IO ()
runTestSuite chattyIO = void . runTestSuiteResult chattyIO

runTestSuiteResult :: Bool -> TestSuite -> IO Result
runTestSuiteResult chattyIO ts = do
   hSetBuffering stdout NoBuffering
   runWriteIO $ do
      result <- runTS ts
      newline
      return result
 where
   runTS :: TestSuite -> WriteIO Result
   runTS t = do 
      (res, dt) <- getDiffTime (foldM addTest mempty (tests t))
      returnStrict res { diffTime = dt }
    
   runTest :: Test -> WriteIO Result
   runTest t =
      case t of
         Suite s xs -> runSuite s xs
         Case s io  -> runTestCase s io
   
   runSuite :: String -> TestSuite -> WriteIO Result
   runSuite s ts = do
      when chattyIO $ do
         newline
         liftIO $ putStrLn s
         reset
      result <- runTS ts
      returnStrict (suiteResult s result)
   
   runTestCase :: String -> IO Message -> WriteIO Result
   runTestCase s io = do
      msg <- liftIO (io `catch` handler)
      case messageStatus msg of
         _ | not chattyIO -> return () 
         Ok -> dot
         _  -> do
            newlineIndent
            liftIO (print msg)
            reset
      returnStrict (caseResult (s, msg))
    where
      handler :: SomeException -> IO Message
      handler = return . message . show

   addTest :: Result -> Test -> WriteIO Result
   addTest res = liftM (res <>) . runTest

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

data Result = Result
   { suites       :: S.Seq (String, Result)
   , cases        :: S.Seq (String, Message)
   , diffTime     :: !NominalDiffTime
   , nrOfTests    :: !Int
   , nrOfWarnings :: !Int
   , nrOfErrors   :: !Int
   , resultRating :: !Rating
   }

-- one-line summary
instance Show Result where
   show result =
      "(tests: "     ++ show (nrOfTests result)    ++
      ", errors: "   ++ show (nrOfErrors result)   ++
      ", warnings: " ++ show (nrOfWarnings result) ++
      ", "           ++ show (diffTime result)     ++ ")"
         
instance Monoid Result where
   mempty = Result mempty mempty 0 0 0 0 mempty
   x `mappend` y = Result 
      { suites       = suites x <> suites y 
      , cases        = cases x  <> cases y
      , diffTime     = diffTime x     + diffTime y
      , nrOfTests    = nrOfTests x    + nrOfTests y
      , nrOfWarnings = nrOfWarnings x + nrOfWarnings y
      , nrOfErrors   = nrOfErrors x   + nrOfErrors y
      , resultRating = resultRating x <> resultRating y
      }

instance HasStatus Result where
   getStatus r | nrOfErrors r   > 0 = Error
               | nrOfWarnings r > 0 = Warning
               | otherwise          = Ok

instance HasRating Result where
   rating   = rating . resultRating
   rate n a = a {resultRating = Rating n}

suiteResult :: String -> Result -> Result
suiteResult s res = mempty 
   { suites       = S.singleton (s, res)
   , nrOfTests    = nrOfTests res
   , nrOfWarnings = nrOfWarnings res
   , nrOfErrors   = nrOfErrors res 
   , resultRating = resultRating res
   }

caseResult :: (String, Message) -> Result
caseResult x@(_, msg) = 
   case getStatus msg of
      Ok      -> new
      Warning -> new { nrOfWarnings = 1 }
      Error   -> new { nrOfErrors   = 1 }
 where
   new = mempty 
      { cases        = S.singleton x
      , nrOfTests    = 1
      , resultRating = messageRating msg 
      }

subResults :: Result -> [(String, Result)]
subResults = toList . suites

topMessages :: Result -> [(String, Message)]
topMessages = toList . cases

allMessages :: Result -> [(String, Message)]
allMessages res =
   topMessages res ++ concatMap (allMessages . snd) (subResults res)

findSubResult :: String -> Result -> Maybe Result
findSubResult name = listToMaybe . recs
 where
   recs = concatMap rec . subResults
   rec (n, t)
      | n == name = [t]
      | otherwise = recs t

justOneSuite :: Result -> Maybe (String, Result)
justOneSuite res =
   case subResults res of
      [x] | S.null (cases res) -> Just x
      _ -> Nothing

timeInterval :: Result -> Double
timeInterval = fromRational . toRational . diffTime

printSummary :: Result -> IO ()
printSummary = putStrLn . makeSummary

makeSummary :: Result -> String
makeSummary result = unlines $
   [ line
   , "Tests    : " ++ show (nrOfTests result)
   , "Errors   : " ++ show (nrOfErrors result)
   , "Warnings : " ++ show (nrOfWarnings result)
   , "" 
   , "Time     : " ++ show (diffTime result)
   , "" 
   , "Suites: "
   ] ++ map f (subResults result)
     ++ [line]
 where
   line = replicate 75 '-'
   f (name, r) = "   " ++ name ++ "   " ++ show r

-----------------------------------------------------
-- Message

data Message = M 
   { messageStatus :: !Status
   , messageRating :: !Rating
   , messageLines  :: [String]
   }
 deriving Eq

instance Show Message where
   show a = st ++ sep ++ msg
    where
      msg = intercalate ", " (messageLines a)
      sep = if null st || null msg then "" else ": " 
      st | isError a             = "error"
         | isWarning a           = "warning"
         | null (messageLines a) = "ok"
         | otherwise             = ""

instance Monoid Message where
   mempty = M mempty mempty mempty
   M s r xs `mappend` M t q ys = M (s <> t) (r <> q) (xs <> ys)

instance HasStatus Message where
   getStatus = messageStatus

instance HasRating Message where
   rating   = rating . messageRating
   rate n a = a {messageRating = Rating n}

message :: String -> Message
message = M Error (Rating 0) . return

warning :: String -> Message
warning = M Warning mempty . return

-----------------------------------------------------
-- Status

data Status = Ok | Warning | Error
   deriving (Eq, Ord)

instance Monoid Status where
   mempty  = Ok
   mappend = max

class HasStatus a where
   getStatus :: a -> Status

isOk, isWarning, isError :: HasStatus a => a -> Bool
isOk      = (== Ok)      . getStatus
isWarning = (== Warning) . getStatus
isError   = (== Error)   . getStatus

-----------------------------------------------------
-- Rating

data Rating = Rating !Int | MaxRating
   deriving (Eq, Ord)
   
instance Monoid Rating where
   mempty  = MaxRating
   mappend = min
   
class HasRating a where
   rating :: a -> Maybe Int
   rate   :: Int -> a -> a

instance HasRating Rating where
   rating (Rating n) = Just n
   rating MaxRating  = Nothing
   rate = const . Rating

-----------------------------------------------------
-- Utility function

getDiffTime :: MonadIO m => m a -> m (a, NominalDiffTime)
getDiffTime action = do
   t0 <- liftIO getCurrentTime
   a  <- action
   t1 <- liftIO getCurrentTime
   return (a, diffUTCTime t1 t0)
   
returnStrict :: Monad m => a -> m a
returnStrict a = a `seq` return a