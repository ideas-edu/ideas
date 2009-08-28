module Domain.Math.Numeric.Generators 
   ( integerGenerator, rationalGenerator
   , ratioGen, ratioGenNonZero, nonZero
   ) where

import Control.Monad
import Common.View
import Domain.Math.Numeric.Views
import Test.QuickCheck
import Data.Ratio
import Domain.Math.Expr
import Domain.Math.Expr.Symbols
import Domain.Math.Expr.Symbolic

-------------------------------------------------------------------
-- Generators

-- tailored towards generating "int" expressions (also prevents 
-- division by zero)
integerGenerator :: Int -> Gen Expr
integerGenerator = symbolGenerator extras numSymbols
 where
   extras n = natGenerator : [ divGen n | n > 0 ]
   divGen n = do
      e1 <- integerGenerator (n `div` 2)
      e2 <- integerGenerator (n `div` 2)
      case (match integerView e1, match integerView e2) of
         (Just a, Just b) 
            | b == 0 -> oneof $ map return
                 [ e1 :/: (e2 + 1), e1 :/: (e2 - 1)
                 , e1 :/: (1 + e2), e1 :/: (1 - e2) 
                 ]
            | a `mod` b == 0 -> do
                 return (e1 :/: e2)
            | otherwise -> do -- change numerator
                i <- arbitrary
                let m1 = fromInteger ((a `mod` b) + i*b)
                    m2 = fromInteger (b - (a `mod` b) + i*b)
                oneof $ map return 
                   [ (e1 - m1) :/: e2, (m1 - e1) :/: e2
                   , (e1 + m2) :/: e2, (m2 + e1) :/: e2
                   ]
         _ -> error "integerGenerator"

rationalGenerator :: Int -> Gen Expr
rationalGenerator = symbolGenerator (const [natGenerator]) (divSymbol:numSymbols)

{- instance Integral a => Arbitrary (Ratio a) where
   arbitrary     = sized (\n -> ratioGen n (n `div` 4))
   coarbitrary r = f (numerator r) . f (denominator r)
    where f = variant . fromIntegral -}
   
-- | Prevents a bias towards small numbers
ratioGen :: Integral a => Int -> Int -> Gen (Ratio a)
ratioGen n m = do 
   a <- choose (-n, n)
   b <- liftM (succ . abs) (choose (-m, m))
   c <- choose (1-b, b-1)
   return (fromIntegral a + (fromIntegral c / fromIntegral b))

ratioGenNonZero :: Integral a => Int -> Int -> Gen (Ratio a)
ratioGenNonZero n m = nonZero (ratioGen n m)

nonZero :: Num a => Gen a -> Gen a
nonZero = liftM (\a -> if a==0 then 1 else a)

numSymbols :: [Symbol]
numSymbols = [plusSymbol, timesSymbol, minusSymbol, negateSymbol]

(.>.) :: View a b -> View a c -> a -> Bool
(va .>. vb) a 
   | a `belongsTo` va = a `belongsTo` vb
   | otherwise        = True

testAll :: IO ()
testAll = do
   let f p gen = quickCheck (forAll (sized gen) p)
       gens    = [integerGenerator, rationalGenerator]
       props   = [ ("p1", integerNormalForm .>. integerView)
                 , ("p2", rationalNormalForm .>. rationalRelaxedForm)
                 , ("p3", rationalRelaxedForm .>. rationalView)
                 , ("p4", integerNormalForm .>. rationalNormalForm)
                 , ("p5", integerView .>. rationalView)
                 ]
   flip mapM_ props $ \(s, p) -> do 
      putStrLn ("*** " ++ s)
      mapM_ (f p) gens

testIntGen = quickCheck $ forAll (sized integerGenerator) (`belongsTo` integerView)

testFrac = quickCheck $ forAll (sized rationalGenerator) p1 

p1 e = 
   (e `belongsTo` fractionForm) == (e `belongsTo` rationalRelaxedForm && not (e `belongsTo` integerNormalForm)) 