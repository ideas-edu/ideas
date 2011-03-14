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
   , FormatLog(..), formatLog
   ) where

import Control.Arrow
import Control.Monad.State
import Data.Foldable (toList)
import Data.List
import Data.Monoid
import Data.Time
import Test.QuickCheck
import qualified Data.Sequence as S

----------------------------------------------------------------
-- Test Suite Monad

-- Integer corresponds to the number of characters on the current line, and
-- is used for formatting
newtype TestSuiteM a = TSM { unTSM :: M a }

type M a = StateT (Int, ResultTree) IO a
type TestSuite = TestSuiteM ()

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
   newline
   liftIO $ putStrLn s
   reset
   (t, td) <- getDiff (withEmptyTree (unTSM m))
   addTree (labeled (s, td) t)

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
   addResult s (toTestResult (maxSuccess args) r)

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

-- local helpers
addAssertion :: TestResult -> String -> IO Bool -> TestSuite
addAssertion msg s io = TSM $ do
   b <- liftIO (io `catch` \_ -> return False)
   if b then do 
      dot
      addResult s (Ok 1)
    else do
      newlineIndent
      liftIO $ putStrLn (s ++ ": " ++ show msg)
      reset
      addResult s msg

addResult :: String -> TestResult -> M ()
addResult s r = addTree (single (s, r))

addTree :: ResultTree -> M ()
addTree t = modify (second (`mappend` t))

withEmptyTree :: M () -> M ResultTree
withEmptyTree m = do
   t0 <- gets snd
   modify (second (const mempty))
   m
   tr <- gets snd
   modify (second (const t0))
   return tr

-- formatting helpers
newline :: M ()
newline = do
   i <- gets fst
   when (i>0) (liftIO $ putChar '\n')
   reset

newlineIndent :: M ()
newlineIndent = do
   newline
   liftIO $ putStr "   "
   modify (first (const 3))

dot :: M ()
dot = do
   i <- gets fst
   unless (i>0 && i<60) newlineIndent
   liftIO $ putChar '.'
   modify (first (+1))

reset :: M ()
reset = modify (first (const 0))

----------------------------------------------------------------
-- Running a test suite

runTestSuite :: TestSuite -> IO ()
runTestSuite s = runTestSuiteResult s >> return ()

runTestSuiteResult :: TestSuite -> IO TestSuiteResult
runTestSuiteResult s = liftM TSR $ getDiff $ liftM snd $
   execStateT (unTSM s >> newline) (0, mempty)

----------------------------------------------------------------
-- Test Suite Result

newtype TestSuiteResult = TSR (ResultTree, NominalDiffTime)

type ResultTree = Tree (String, NominalDiffTime) (String, TestResult)

data TestResult = Ok !Int | Error String | Warning String

instance Show TestResult where
   show (Ok _)        = "Ok"
   show (Error msg)   = "Error: "   ++ msg
   show (Warning msg) = "Warning: " ++ msg

-- one-line summary
instance Show TestSuiteResult where
   show (TSR (tree, diff)) = 
      let (n, nf, nw) = collectInfo tree
      in "(tests: " ++ show n ++ ", failures: " ++ show nf ++
         ", warnings: " ++ show nw ++ ", " ++ show diff ++ ")"

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
   , "\nTime     : " ++ show diff
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
               let (ys, zs) = span (isOk . snd) list
                   sucs     = [ (x, n) | (x, Ok n) <- ys ]
               in formatSuccesses fm sucs `mappend` forTests zs
       where
         next a = a `mappend` forTests rest

data FormatLog a = FormatLog
   { formatRoot      :: NominalDiffTime -> a -> a
   , formatSuite     :: [Int] -> String -> (Int, Int, Int) -> NominalDiffTime -> a -> a
   , formatSuccesses :: [(String, Int)] -> a
   , formatFailure   :: String -> String -> a
   , formatWarning   :: String -> String -> a
   }

formatLog :: FormatLog [String]
formatLog = FormatLog
   { formatRoot = \td a -> 
        a ++ ["\n(Total time: " ++ show td ++ ")"]
   , formatSuite = \loc s _ td a -> 
        [showLoc loc ++ ". " ++ s] ++ a ++ 
        ["  (" ++ show td ++ " for " ++ s ++ ")"]
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

-----------------------------------------------------
-- Utility functions

-- A sequence of leafs (Left) or labeled items (Right)
newtype Tree a b = T { unT :: S.Seq (Either b (a, Tree a b)) }

instance Monoid (Tree a b) where
   mempty = T mempty
   mappend (T a) (T b) = T (mappend a b)
  
single :: b -> Tree a b
single = T . S.singleton . Left

labeled :: a -> Tree a b -> Tree a b
labeled a t = T (S.singleton (Right (a, t)))
  
toTestResult :: Int -> Result -> TestResult
toTestResult n result = 
   case result of
      Success _ _ _           -> Ok n
      Failure _ _ _ _ msg _ _ -> Error msg
      NoExpectedFailure _ _ _ -> Error "no expected failure"
      GaveUp i _  _           -> Warning ("passed only " ++ show i ++ " tests")
            
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
collectLevel = combine [] . toList . unT
 where
   combine acc []             = f acc
   combine acc (Left a:rest)  = combine (a:acc) rest
   combine acc (Right b:rest) = f acc ++ (Right b : combine [] rest)
   
   f acc = [ Left (reverse acc) | not (null acc) ] 

getDiff :: MonadIO m => m a -> m (a, NominalDiffTime)
getDiff action = do
   t0 <- liftIO getCurrentTime
   a  <- action
   t1 <- liftIO getCurrentTime
   return (a, diffUTCTime t1 t0)

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
         assertTrue "yes" True
         
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