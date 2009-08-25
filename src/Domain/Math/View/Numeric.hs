module Domain.Math.View.Numeric 
   ( integralView, realView
   , integerView, rationalView, doubleView
   , integerNormalForm, rationalNormalForm, rationalRelaxedForm, fractionForm
   , integerGenerator, rationalGenerator
   ) where

import Common.View
import Control.Monad
import Data.Ratio
import Domain.Math.Expr.Symbolic
import Domain.Math.Expr.Symbols
import Domain.Math.Expr
import Test.QuickCheck

-------------------------------------------------------------------
-- Numeric views

integralView :: Integral a => View Expr a
integralView = makeView (exprToNum f) fromIntegral
 where
   f s [x, y] | s == divSymbol = intDiv x y
   f _ _ = Nothing

realView :: RealFrac a => View Expr a
realView = makeView (exprToNum f) (fromRational . toRational)
 where
   f s [x, y] | s == divSymbol = fracDiv x y
   f _ _ = Nothing
   
integerView :: View Expr Integer
integerView = integralView

rationalView :: View Expr Rational
rationalView = makeView (match realView) fromRational

-- No floating view
doubleView :: View Expr Double
doubleView = makeView (exprToNum doubleSym)
                      (fromRational . flip approxRational 0.0001)
 
-------------------------------------------------------------------
-- Numeric views in normal form 

-- N or -N (where n is a natural number)
integerNormalForm :: View Expr Integer
integerNormalForm = makeView (optionNegate f) fromInteger
 where
   f (Nat n) = Just n
   f _       = Nothing

rationalNormalForm :: View Expr Rational
rationalNormalForm = makeView (optionNegate f) fromRational
 where   
   f (Nat a :/: Nat b) = do
      guard (a > 0 && b > 1 && gcd a b == 1)
      Just (fromInteger a / fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

fractionForm :: View Expr (Integer, Integer)
fractionForm = makeView f (\(a, b) -> (fromInteger a :/: fromInteger b))
 where
   f (Negate a) = liftM (\(x,y) -> (negate x, y)) (g a)
   f a = g a
   g (e1 :/: e2) = do
      a <- match integerNormalForm e1
      b <- match integerNormalForm e2
      guard (b /= 0)
      return (a, b)
   g _       = Nothing

rationalRelaxedForm :: View Expr Rational
rationalRelaxedForm = makeView (optionNegate f) fromRational
 where
   f (e1 :/: e2) = do
      a <- match integerNormalForm e1
      b <- match integerNormalForm e2
      fracDiv (fromInteger a) (fromInteger b)
   f (Nat n) = Just (fromInteger n)
   f _       = Nothing

-- helper-function
optionNegate :: (MonadPlus m, Num a) => (Expr -> m a) -> Expr -> m a
optionNegate f (Negate a) = do b <- f a; guard (b /= 0); return (negate b)
optionNegate f a          = f a

-------------------------------------------------------------------
-- Generators

-- tailored towards generating "int" expressions (also prevents 
-- division by zero)
integerGenerator :: Int -> Gen Expr
integerGenerator = symbolGeneratorWith extras [] numSymbols
 where
   extras n = [ divGen n | n > 0 ]
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
rationalGenerator = symbolGenerator [] (divSymbol:numSymbols)

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

-------------------------------------------------------------------
-- Helper functions

doubleSym :: Symbol -> [Double] -> Maybe Double
doubleSym s [x, y] | s == divSymbol  = fracDiv x y
doubleSym s [x]    | s == sqrtSymbol && x >= 0 = Just (sqrt x)
doubleSym _ _ = Nothing

-- General numeric interpretation function: constructors Sqrt and
-- (:/:) are interpreted with function
exprToNum :: (Monad m, Num a) => (Symbol -> [a] -> m a) -> Expr -> m a
exprToNum f = foldExpr 
   ( liftM2 (+)
   , liftM2 (*)
   , liftM2 (-)
   , liftM negate
   , return . fromInteger
   , \mx my -> do x <- mx; y <- my; f divSymbol [x, y]
   , \mx    -> do x <- mx; f sqrtSymbol [x]
   , \_     -> fail "exprToNum: variable"
   , \s xs  -> sequence xs >>= f s
   )

intDiv :: Integral a => a -> a -> Maybe a
intDiv x y 
   | y /= 0 && m == 0 = Just d
   | otherwise        = Nothing
 where (d, m) = x `divMod` y
 
fracDiv :: Fractional a => a -> a -> Maybe a
fracDiv x y 
   | y /= 0    = Just (x / y)
   | otherwise = Nothing