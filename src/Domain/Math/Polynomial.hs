module Domain.Math.Polynomial where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Char

-- Invariants: all keys are non-negative, all values are non-zero
newtype Polynomial a = P (IM.IntMap a) deriving Eq

instance Num a => Show (Polynomial a) where
   show (P m) = 
      let f (n, a) = sign (one (show a ++ g n))
          g n = concat $ [ "x" | n > 0 ] ++ [ "^" ++ show n | n > 1 ]
          one ('1':xs@('x':_))     = xs
          one ('-':'1':xs@('x':_)) = xs
          one xs                   = xs
          sign ('-':xs) = " - " ++ xs
          sign xs       = " + " ++ xs
          fix xs = case dropWhile isSpace xs of
                      '+':ys -> dropWhile isSpace ys
                      '-':ys -> '-':dropWhile isSpace ys
                      ys     -> ys
      in "f(x) = " ++ 
         if IM.null m then "0" else 
             fix (concatMap f (reverse (IM.toList m)))

-- the Functor instance does not maintain the invariant
instance Functor Polynomial where
   fmap f (P m) = P (IM.map f m)

instance Num a => Num (Polynomial a) where
   P m1 + P m2   = P (IM.filter (/= 0) (IM.unionWith (+) m1 m2))
   p    * P m2   = IM.foldWithKey op 0 m2
    where op n a m = raise n (scale a p) + m
   negate         = fmap negate
   fromInteger n
      | n == 0    = P IM.empty
      | otherwise = P (IM.singleton 0 (fromInteger n))
   -- not defined for polynomials
   abs    = error "abs not defined for polynomials"
   signum = error "signum not defined for polynomials"

-- a single variable (such as "x") 
var :: Num a => Polynomial a
var = P (IM.singleton 1 1)

con :: a -> Polynomial a
con a = P (IM.singleton 0 a)

-- | Raise all powers by a (non-negative) constant
raise :: Int -> Polynomial a -> Polynomial a
raise i (P m)
   | i >= 0    = P $ IM.fromAscList [ (n+i, a) | (n, a) <- IM.toList m ]
   | otherwise = error "raise with a negative number"

power :: Num a => Polynomial a -> Int -> Polynomial a
power _ 0 = 1
power p n = p * power p (n-1)

scale :: Num a => a -> Polynomial a -> Polynomial a
scale a p = if a==0 then 0 else fmap (*a) p

------------------------------------------------

degree :: Polynomial a -> Int
degree (P m)
   | IS.null is = 0
   | otherwise  = IS.findMax (IM.keysSet m)
 where is = IM.keysSet m

coefficient :: Num a => Int -> Polynomial a -> a
coefficient n (P m) = IM.findWithDefault 0 n m

isMonic :: Num a => Polynomial a -> Bool
isMonic p = coefficient (degree p) p == 1

toMonic :: Fractional a => Polynomial a -> Polynomial a
toMonic p = scale (recip a) p
 where a = coefficient (degree p) p

-- Returns the maximal number of positive roots (Descartes theorem)
-- Multiple roots are counted separately
positiveRoots :: Num a => Polynomial a -> Int
positiveRoots (P m) = signChanges (IM.elems m)

-- Returns the maximal number of negative roots (Descartes theorem)
-- Multiple roots are counted separately
negativeRoots :: Num a => Polynomial a -> Int
negativeRoots (P m) = signChanges (flipOdd (IM.elems m))
 where 
   flipOdd (x:y:zs) = x:negate y:flipOdd zs
   flipOdd xs = xs

signChanges :: Num a => [a] -> Int
signChanges = f . map signum
 where
   f (x:xs@(hd:_)) = if x==hd then f xs else 1 + f xs
   f _ = 0
   
------------------------------------------------

derivative :: Num a => Polynomial a -> Polynomial a 
derivative (P m) = P $ IM.fromAscList 
   [ (n-1, fromIntegral n*a) | (n, a) <- IM.toList m, n > 0 ]

eval :: Num a => Polynomial a -> a -> a
eval (P m) x = sum [ a * x^n | (n, a) <- IM.toList m ] 

-- polynomial division, no remainder
division :: Fractional a => Polynomial a -> Polynomial a -> Maybe (Polynomial a)
division p1 p2 = if b==0 then return a else Nothing 
 where (a, b) = longDivision p1 p2

-- polynomial long division
longDivision :: Fractional a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
longDivision p1 p2 = monicLongDivision (scale (recip a) p1) (scale (recip a) p2)
 where a = coefficient (degree p2) p2

-- polynomial long division, where p2 is monic
monicLongDivision :: Num a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
monicLongDivision p1 p2
   | d1 >= d2 && isMonic p2 = (toP quot, toP rem)
   | otherwise = error "invalid monic division"
 where
   d1 = degree p1
   d2 = degree p2
   xs = map (`coefficient` p1) [d1, d1-1 .. 0]
   ys = drop 1 $ map (negate . (`coefficient` p2)) [d2, d2-1 .. 0]
   
   (quot, rem) = rec [] xs
   toP = P . IM.filter (/= 0) . IM.fromAscList . zip [0..]
   
   rec acc (a:as) | length as >= length ys = 
      rec (a:acc) (zipWith (+) (map (*a) ys ++ repeat 0) as)
   rec acc as = (acc, reverse as)
   
-- use polynomial long division to compute the greatest common factor 
-- of the polynomials
polynomialGCD :: Fractional a => Polynomial a -> Polynomial a -> Polynomial a
polynomialGCD x y
   | degree y > degree x = rec y x 
   | otherwise           = rec x y
 where
   rec a b
      | b == 0    = a 
      | otherwise = rec b (snd (longDivision a b))
   
------------------------

-- (x+5)(x+2)(x-3)(x-7)(x-8)
test = longDivision e1 e2 

test2 = division e3 ((var + 2) * (var - 3))

e3 = (var+5) * (var + 2) * (var - 3) * (var - 7) * (var - 8)

e1 = raise 2 var - 12 * raise 1 var - 42
e2 = 2 * (raise 1 var + var - 3)
 
f :: Rational -> Rational
f x = x*x + 2*x - 3

ff :: Rational -> Rational
ff x = 2*x + 2

next :: Rational -> Rational
next a = a - (f a / ff a)

newton = take 100 (iterate next (-10))

e8, e9 :: Polynomial Rational
e8 = (var+1) * (var-5) * (var-3)
e9 = (var-2) * (var-5) * (var+2) * (var+2)

(q0, r0) = longDivision e8 e9
(q1, r1) = longDivision e9 r0
(q2, r2) = longDivision r0 r1 -- r1 is wat ik zoek

