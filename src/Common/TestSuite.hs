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
   , assertTrue, assertTrueMsg, assertNull, assertEquals, assertIO
     -- * Running a test suite
   , runTestSuite, runTestSuiteResult
     -- * Test Suite Result
   , TestSuiteResult, subResults
   , makeSummary, printSummary
   , makeTestLog, printTestLog
   ) where

import Data.List
import Test.QuickCheck
import System.Random
import Common.Utils (commaList, thd3)
import Control.Monad.State
import System.Time

----------------------------------------------------------------
-- Test Suite Monad

newtype TestSuiteM a = TSM { unTSM :: StateT TestState IO a }

type TestSuite = TestSuiteM ()  
type TestState = Tree String Test

data Test = QuickCheck String Args Property
          | Assert     String (IO Bool) String

instance Monad TestSuiteM where
   return  = TSM . return
   m >>= f = TSM (unTSM m >>= unTSM . f)

instance MonadIO TestSuiteM where
   liftIO =  TSM . liftIO

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
assertTrueMsg s msg = addAssertion msg s . return

assertNull :: Show a => String -> [a] -> TestSuite
assertNull s xs = addAssertion msg s (return (null xs))
 where msg = commaList (map show xs)
 
assertEquals :: (Eq a, Show a) => String -> a -> a -> TestSuite
assertEquals s x y = addAssertion msg s (return (x==y))
 where msg = "Not equal: " ++ show x ++ " and " ++ show y

assertIO :: String -> IO Bool -> TestSuite
assertIO = addAssertion "Assertion failed"

-- local helper
addAssertion :: String -> String -> IO Bool -> TestSuite
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

   forTests ts = do
      runTests False [] ts
         
   runTests b acc [] = do
      when b (putChar '\n')
      return (foldr ((:+:) . Single) Empty (reverse acc))
   
   runTests b acc (QuickCheck s args p : rest) = do
      putStr $ [ '\n' | b ] ++ "   "
      unless (null s) $ putStr (s ++ ": ")
      r <- quickCheckWithResult args p
      runTests False ((s, maxSuccess args, r) : acc) rest
   
   runTests b acc (Assert s io msg : rest) = io >>= \ok ->
      if ok then do
         putStr $ if b then "." else "   ."
         runTests True ((s, 1, success) : acc) rest
         else do
         when b $ putStr "\n   "
         unless (null s) $ putStr $ s ++ ": "
         putStrLn msg
         runTests False ((s, 1, failure msg) : acc) rest

----------------------------------------------------------------
-- Test Suite Result

newtype TestSuiteResult = TSR (ResultTree, TimeDiff)

type ResultTree = Tree (String, TimeDiff) (String, Int, Result)

-- one-line summary
instance Show TestSuiteResult where
   show (TSR (tree, diff)) = 
      let (n, nf, nw) = collectInfo tree
      in "(tests: " ++ show n ++ ", failures: " ++ show nf ++
         ", warnings: " ++ show nw ++ ", " ++ formatDiff diff ++ ")"

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
   , "\nTime     : " ++ formatDiff diff
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
makeTestLog (TSR (tree, diff)) = unlines (make [] tree) ++ totalTime
 where
   totalTime  = "\n(Total time: " ++ formatDiff diff ++ ")"
   make loc t = concatMap (either (map forTest) forSuite) xs
    where
      xs = reverse $ snd $ foldl op (1::Int, []) (collectLevel t)
      op (i, ys) y = case y of 
                        Left b  -> (i, Left b:ys)
                        Right p -> (i+1, Right (loc ++ [i], p):ys)
      
      forSuite (nl, ((s, d), t)) = 
         (showLoc nl ++ ". " ++ s) : make nl t 
         ++ ["  (" ++ formatDiff d ++ " for " ++ s ++ ")"]
      forTest (s, n, r)
         | null s    = result
         | otherwise = "  " ++ s ++ ": " ++ result
       where
         result = 
            case r of
               Success _
                  | n > 1     -> "Ok (" ++ show n ++ " tests)"
                  | otherwise -> "Ok "
               GaveUp n _ -> "Warning: passed only " ++ show n ++ " tests."
               Failure _ _ msg _   -> "Error: " ++ msg
               NoExpectedFailure _ -> "Error: no expected failure" 

-----------------------------------------------------
-- Utility functions

data Tree a b = Labeled a (Tree a b) 
              | Tree a b :+: Tree a b
              | Empty
              | Single b

success :: Result
success = Success []

failure :: String -> Result
failure s = Failure (mkStdGen 0) 0 s []
         
showLoc :: [Int] -> String
showLoc = concat . intersperse "." . map show

collectInfo :: Tree a (String, Int, Result) -> (Int, Int, Int)
collectInfo tree = (length tests, length failures, length warnings)
 where
   tests    = flatten tree
   failures = filter (isFailure . thd3) tests
   warnings = filter (isWarning . thd3) tests

   isFailure (Failure _ _ _ _)     = True
   isFailure (NoExpectedFailure _) = True
   isFailure _ = False
   
   isWarning (GaveUp _ _) = True
   isWarning _ = False

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

formatDiff :: TimeDiff -> String
formatDiff d@(TimeDiff z1 z2 z3 h m s p)
   | any (/=0) [z1,z2,z3] = timeDiffToString d
   | s >= 60      = formatDiff (timeDiff ((h*60+m)*60+s) p)
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

-- Example

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
         assertEquals "eq" (sort [1,2,2]) (nub [1,2,2]) 
   printSummary r
   --printTestLog r
   --print r
   --print (subResults r)
 where      
   p1 xs = sort (xs::[Int]) == sort (sort xs)
   p2 xs = reverse (reverse xs) == (xs::[Int])
   p3 xs = head (sort xs) == minimum (xs::[Int])
   p4 xs = sort (nub xs) == nub (sort (xs::[Int]))
   p5 xs = reverse (sort xs) == sort (reverse (xs :: [Int]))