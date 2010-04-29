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
   ( -- * Test Suite Monad
     TestSuite, MonadIO(..)
     -- * Test suite constructors
   , suite, addProperty, addPropertyWith
   , assertTrue, assertTrueMsg, assertNull, assertEquals
   , assertIO, warn
     -- * Running a test suite
   , runTestSuite, runTestSuiteResult
     -- * Test Suite Result
   , TestSuiteResult, subResults
   , makeSummary, printSummary
   , makeTestLog, makeTestLogWith, printTestLog
     -- * Formatting
   , FormatLog(..), formatLog, formatTimeDiff
   ) where

import Data.List
import Data.Monoid
import Test.QuickCheck
import Control.Monad.State
import System.Time hiding (formatTimeDiff)

----------------------------------------------------------------
-- Test Suite Monad

newtype TestSuiteM a = TSM { unTSM :: StateT TestState IO a }

type TestSuite = TestSuiteM ()  
type TestState = Tree String Test

data Test = QuickCheck String Args Property
          | Assert     String (IO Bool) TestResult

instance Monad TestSuiteM where
   return  = TSM . return
   m >>= f = TSM (unTSM m >>= unTSM . f)
   fail s  = do assertTrueMsg "" s False
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
   tree <- liftIO $ execStateT (unTSM m) Empty
   let new = Labeled s tree
   modify (:+: new)

-- | Add a QuickCheck property to the test suite. The first argument is 
-- a label for the property
addProperty :: Testable prop => String -> prop -> TestSuite
addProperty = flip addPropertyWith stdArgs

-- | Add a QuickCheck property to the test suite, also providing a test
-- configuration (Args)
addPropertyWith :: Testable prop => String -> Args -> prop -> TestSuite
addPropertyWith s args p = TSM (modify (:+: new))
 where
   new = Single (QuickCheck s args (property p))

assertTrue :: String -> Bool -> TestSuite
assertTrue msg = assertIO msg . return

assertTrueMsg :: String -> String -> Bool -> TestSuite
assertTrueMsg s msg = addAssertion (Error msg) s . return

assertNull :: Show a => String -> [a] -> TestSuite
assertNull s xs = addAssertion (f xs) s (return (null xs))
 where f = Error . concat . intersperse "\n" . map show
 
assertEquals :: (Eq a, Show a) => String -> a -> a -> TestSuite
assertEquals s x y = addAssertion msg s (return (x==y))
 where msg = Error ("Not equal: " ++ show x ++ " and " ++ show y)

assertIO :: String -> IO Bool -> TestSuite
assertIO = addAssertion (Error "Assertion failed")

warn :: String -> TestSuite
warn msg = addAssertion (Warning msg) "" (return False)

-- local helper
addAssertion :: TestResult -> String -> IO Bool -> TestSuite
addAssertion msg s io = TSM $ modify (:+: Single (Assert s io msg))

----------------------------------------------------------------
-- Running a test suite

runTestSuite :: TestSuite -> IO ()
runTestSuite suite = runTestSuiteResult suite >> return ()

runTestSuiteResult :: TestSuite -> IO TestSuiteResult
runTestSuiteResult suite = liftM TSR $ getDiff $ do
   tree <- execStateT (unTSM suite) Empty
   run tree
 where
   run tree = do
      xs <- mapM (either forTests forSuite) (collectLevel tree)
      return (foldr (:+:) Empty xs)

   forSuite (s, ts) = do 
      (t, diff) <- getDiff (putStrLn (s ++ ":") >> run ts)
      return (Labeled (s, diff) t)

   forTests = rec False []
    where
      rec b acc [] = do
         when b (putChar '\n')
         return (foldr ((:+:) . Single) Empty (reverse acc))

      rec b acc (QuickCheck s args p : rest) = do
         putStr $ [ '\n' | b ] ++ "   "
         unless (null s) $ putStr (s ++ ": ")
         r <- quickCheckWithResult args p
         rec False ((s, toTestResult (maxSuccess args) r) : acc) rest

      rec b acc (Assert s io msg : rest) = io >>= \ok ->
         if ok then do
            putStr $ if b then "." else "   ."
            rec True ((s, Ok 1) : acc) rest
            else do
            when b $ putChar '\n'
            putStr "   "
            unless (null s) $ putStr $ s ++ ": "
            print msg
            rec False ((s, msg) : acc) rest

----------------------------------------------------------------
-- Test Suite Result

newtype TestSuiteResult = TSR (ResultTree, TimeDiff)

type ResultTree = Tree (String, TimeDiff) (String, TestResult)

data TestResult = Ok Int | Error String | Warning String

instance Show TestResult where
   show (Ok _)        = "Ok"
   show (Error msg)   = "Error: "   ++ msg
   show (Warning msg) = "Warning: " ++ msg

-- one-line summary
instance Show TestSuiteResult where
   show (TSR (tree, diff)) = 
      let (n, nf, nw) = collectInfo tree
      in "(tests: " ++ show n ++ ", failures: " ++ show nf ++
         ", warnings: " ++ show nw ++ ", " ++ formatTimeDiff diff ++ ")"

subResults :: TestSuiteResult -> [(String, TestSuiteResult)]
subResults (TSR (tree, _)) = 
   let f ((s, diff), t) = (s, TSR (t, diff))
   in map f (subtrees tree)

printSummary :: TestSuiteResult -> IO ()
printSummary = putStrLn . makeSummary

makeSummary :: TestSuiteResult -> String
makeSummary result@(TSR (tree, diff)) = unlines $
   [ line
   , "Tests    : " ++ show n
   , "Failures : " ++ show nf
   , "Warnings : " ++ show nw
   , "\nTime     : " ++ formatTimeDiff diff
   , "\nSuites: "
   ] ++ map f (subResults result) 
     ++ [line]
 where
   line        = replicate 75 '-'
   (n, nf, nw) = collectInfo tree
   f (name, r) = "   " ++ name ++ "   " ++ show r

printTestLog :: TestSuiteResult -> IO ()
printTestLog = putStrLn . makeTestLog

makeTestLog :: TestSuiteResult -> String
makeTestLog = unlines . makeTestLogWith formatLog

makeTestLogWith :: Monoid a => FormatLog a -> TestSuiteResult -> a
makeTestLogWith fm (TSR (tree, diff)) = formatRoot fm diff (make [] tree)
 where
   make loc = mconcat . map (either forTests forSuite) . treeToList
    where
      treeToList = 
         let op (i, ys) y = 
                case y of 
                   Left b  -> (i, Left b:ys)
                   Right p -> (i+1, Right (loc ++ [i], p):ys)
         in reverse . snd . foldl op (1, []) . collectLevel

      forSuite (nl, ((s, d), t)) = 
         formatSuite fm nl s (collectInfo t) d (make nl t)
      
      forTests [] = mempty
      forTests list@((s, result) : rest) = 
         case result of            
            Warning msg -> next (formatWarning fm s msg)
            Error msg   -> next (formatFailure fm s msg)
            Ok _        ->
               let (ys, zs) = break (not . isOk . snd) list
                   sucs     = [ (s, n) | (s, Ok n) <- ys ]
               in formatSuccesses fm sucs `mappend` forTests zs
       where
         next a = a `mappend` forTests rest

data FormatLog a = FormatLog
   { formatRoot      :: TimeDiff -> a -> a
   , formatSuite     :: [Int] -> String -> (Int, Int, Int) -> TimeDiff -> a -> a
   , formatSuccesses :: [(String, Int)] -> a
   , formatFailure   :: String -> String -> a
   , formatWarning   :: String -> String -> a
   }

formatLog :: FormatLog [String]
formatLog = FormatLog
   { formatRoot = \td a -> 
        a ++ ["\n(Total time: " ++ formatTimeDiff td ++ ")"]
   , formatSuite = \loc s _ td a -> 
        [showLoc loc ++ ". " ++ s] ++ a ++ 
        ["  (" ++ formatTimeDiff td ++ " for " ++ s ++ ")"]
   , formatSuccesses = \xs -> 
        let f (_, n) = if n==1 then "." else "(" ++ show n ++ " tests)"
        in ["   " ++ concatMap f xs]
   , formatFailure = \s msg ->
        ["   " ++ putLabel s ++ "Error: " ++ msg]
   , formatWarning = \s msg ->
        ["   " ++ putLabel s ++ "Warning: " ++ msg]
   }
 where 
   putLabel s = if null s then "" else s ++ ": "

formatTimeDiff :: TimeDiff -> String
formatTimeDiff d@(TimeDiff z1 z2 z3 h m s p)
   | any (/=0) [z1,z2,z3] = timeDiffToString d
   | s >= 60      = formatTimeDiff (timeDiff ((h*60+m)*60+s) p)
   | h==0 && m==0 = show inSec ++ " secs"
   | otherwise    = show (60*h+m) ++ ":" ++ digSec ++ " mins" 
 where
   milSec = 1000*toInteger s + p `div` 1000000000
   inSec  = fromIntegral milSec / 1000
   digSec = (if s < 10 then ('0' :) else id) (show s)
   timeDiff n p = 
      let (rest, s) = n `divMod` 60
          (h, m)    = rest `divMod` 60
      in TimeDiff 0 0 0 h m s p

-----------------------------------------------------
-- Utility functions

data Tree a b = Labeled a (Tree a b) 
              | Tree a b :+: Tree a b
              | Empty
              | Single b
        
toTestResult :: Int -> Result -> TestResult
toTestResult n result = 
   case result of
      Success _           -> Ok n
      Failure _ _ msg _   -> Error msg
      NoExpectedFailure _ -> Error "no expected failure"
      GaveUp n _          -> Warning ("passed only " ++ show n ++ " tests")
            
showLoc :: [Int] -> String
showLoc = concat . intersperse "." . map show

collectInfo :: Tree a (String, TestResult) -> (Int, Int, Int)
collectInfo tree = (length tests, length failures, length warnings)
 where
   tests    = flatten tree
   failures = [ msg | (_, Error msg)   <- tests ]
   warnings = [ msg | (_, Warning msg) <- tests ]

isOk :: TestResult -> Bool
isOk (Ok _) = True
isOk _      = False

subtrees :: Tree a b -> [(a, Tree a b)]
subtrees t = [ p | Right p <- collectLevel t ]

flatten :: Tree a b -> [b]
flatten t = [ b | x <- collectLevel t, b <- either id (flatten . snd) x ]

collectLevel :: Tree a b -> [Either [b] (a, Tree a b)]
collectLevel = combine [] . ($ []) . rec
 where
   combine acc [] = left acc
   combine acc (Left a:rest) = combine (a:acc) rest
   combine acc (Right b:rest) = left acc ++ (Right b : rest)
   
   left acc = [ Left (concat (reverse acc)) | not (null acc) ] 
 
   rec tree =
      case tree of
         Labeled a t -> (Right (a, t):)
         Single b    -> (Left [b]:) 
         s :+: t     -> rec s . rec t
         Empty       -> id

getDiff :: IO a -> IO (a, TimeDiff)
getDiff action = do
   t0 <- liftIO getClockTime
   a  <- action
   t1 <- liftIO getClockTime
   return (a, diffClockTimes t1 t0)

-- Example
{-
main :: IO ()
main = do
   r <- runTestSuiteResult $ do
      suite "A" $ do
         addProperty "p1" p1
         addProperty "p1" p1
         suite "A1" $ addProperty "p2" p2
         suite "A2" $ return ()
         addProperty "p3" p3
      suite "B" $ do
         addProperty "p4" p4
         addProperty "W" (\xs -> length (xs::[Int]) > 100 ==> True)
      suite "C" $ do
         addProperty "p5" p5
         assertTrue "sorted" (sort [3,2,1] == [1,2,3])
         fail "This is a failure"
         warn "This is a warning"
         assertEquals "eq" (sort [1,2,2]) (nub [1,2,2]) 
         
   printSummary r
   printTestLog r
   --print r
   --print (subResults r)
 where      
   p1 xs = sort (xs::[Int]) == sort (sort xs)
   p2 xs = reverse (reverse xs) == (xs::[Int])
   p3 xs = head (sort xs) == minimum (xs::[Int])
   p4 xs = sort (nub xs) == nub (sort (xs::[Int]))
   p5 xs = reverse (sort xs) == sort (reverse (xs :: [Int])) -}