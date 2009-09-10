module Domain.Math.SquareRoot.Tests (tests) where

import Control.Monad
import Test.QuickCheck
import Domain.Math.Data.SquareRoot
import Domain.Math.Numeric.Laws

-------------------------------------------------------------------
-- Testing
 
tests :: IO ()
tests = do 
   testNumLaws  "square roots" squareRootGen
   -- 	testFracLaws "square roots" squareRootGen

squareRootGen :: Gen (SquareRoot Rational)
squareRootGen = do
   n <- choose (0, 10)
   let f r1 r2 = fromRational r1 * sqrtRational (abs r2)
   ps <- replicateM n $ liftM2 f arbitrary arbitrary
   return (sum ps)