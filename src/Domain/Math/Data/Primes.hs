-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Calculating prime numbers and prime factors
-- 
-----------------------------------------------------------------------------
module Domain.Math.Data.Primes 
   ( primes, isPrime, coprime, primeFactors, factors
   , testPrimes
   ) where

import Common.Utils.TestSuite
import Control.Monad
import Data.Function
import Data.List
import qualified Data.Sequence as S
import Test.QuickCheck

------------------------------------------------------------
-- | All prime numbers smaller than 1000

-- | An infinite list of prime numbers
primes :: [Int]
primes = 1 : 2 : 3 : 5 : sieve (candidates 7)

-- | All prime factors of a number
primeFactors :: Int -> [Int]
primeFactors n
   | n > 0     = rec (tail primes1000) n
   | otherwise = error "primeFactors: non-positive argument"
 where
   rec [] a 
      | a < 1000000 = [a] -- primes up to 1000 have been checked 
      | otherwise   = sort (rhos a)
   rec list@(p:ps) a 
      | a == 1    = []
      | m == 0    = p : rec list d
      | otherwise = rec ps a
    where
      (d, m) = a `divMod` p

   rhos a = 
      case pollardsRho a of
         Just d  -> rhos d ++ rhos (a `div` d)
         Nothing -> [a] -- probably a prime

primes1000 :: [Int]
primes1000 = 
   [1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97
   ,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193
   ,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307
   ,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421
   ,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547
   ,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659
   ,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797
   ,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929
   ,937,941,947,953,967,971,977,983,991,997]

-- Pollard's rho algorithm
--    see http://en.wikipedia.org/wiki/Pollard_rho
pollardsRho :: Int -> Maybe Int
pollardsRho n = msum (map try [1..10]) -- ten attempts
 where
   try :: Int -> Maybe Int
   try c = rec 2 2 1
    where
      rec :: Int -> Int -> Int -> Maybe Int
      rec x y d
         | d == 1    = rec nx ny (abs (nx-ny) `gcd` n)
         | d == n    = Nothing
         | otherwise = Just d -- a non-trivial factor of n
       where
         nx = f x
         ny = f (f y)
         
      f :: Int -> Int
      f x = (x*x+c) `mod` n 

-- | Testing for primality
isPrime :: Int -> Bool
isPrime a =
   case primeFactors a of
      b:_ -> a == b
      _   -> True

-- | Two numbers are coprime if they do not share a prime factor
coprime :: Int -> Int -> Bool
coprime = rec `on` primeFactors
 where
   rec xs@(x:xr) ys@(y:yr) = 
      case compare x y of
         LT -> rec xr ys
         EQ -> False
         GT -> rec xs yr
   rec _ _ = True

-- | All factors of a number
factors :: Int -> [Int]
factors = sort . rec . primeFactors
 where
   rec []     = [1]
   rec (x:xs) = [ a*b | b <- take n (powers x), a <- rec zs ]
    where
      (ys, zs) = break (/= x) xs
      n = 2 + length ys

-- helper functions
sieveSlow :: [Int] -> [Int]
sieveSlow []     = []
sieveSlow (x:xs) = x : sieveSlow (filter (noDivisorOf x) xs)

sieve :: [Int] -> [Int]
sieve = rec S.empty 
 where
   rec _ [] = []
   rec q (x:xs) = 
      case S.viewl q of 
         (y:ys) S.:< qr | x == y -> 
            rec qr (ys `removeFrom` xs)
         _ -> x : rec (q S.|> map (*x) (candidates x)) xs

   -- remove a sorted list from another list
   removeFrom xs@(x:xr) ys@(y:yr) = 
      case compare x y of 
         LT -> removeFrom xr ys
         EQ -> removeFrom xr yr
         GT -> y:removeFrom xs yr
   removeFrom _ _ = []

-- infinite list starting from n, without factors of 2, 3, or 5
candidates :: Int -> [Int] 
candidates n = dropWhile (< n)
   [ 30*k+i | k <- [n `div` 30..], i <- [1,7,11,13,17,19,23,29] ]

divisorOf :: Int -> Int -> Bool
divisorOf x y = y `mod` x == 0

noDivisorOf :: Int -> Int -> Bool
noDivisorOf x y = y `mod` x /= 0

powers :: Int -> [Int]
powers a = iterate (*a) 1

-- a trusted implementation
primesSlow :: [Int]
primesSlow = 1 : 2 : sieveSlow [3, 5 ..]

testPrimes :: TestSuite
testPrimes = suite "primes" $ do
   assertTrue "first 1000 primes" (take 1000 primesSlow == take 1000 primes)
   assertTrue "isPrime" (all isPrime primes1000)
   addProperty "product of prime factors" $ 
      forAll (choose (1, 1000000)) $ \n -> 
      product (primeFactors n) == n
   addProperty "primality of prime factors" $ 
      forAll (choose (1, 1000000)) $ \n -> 
      all isPrime (primeFactors n)
   addProperty "factoring product of two primes" $ 
      forAll (elements $ tail primes1000) $ \a ->
      forAll (elements $ tail primes1000) $ \b ->  
      primeFactors (a*b) == sort [a, b]
   addProperty "factors" $ 
      forAll (choose (1, 10000)) $ \n -> 
      all (`divisorOf` n) (factors n)
   addProperty "factors of product" $ 
      forAll (choose (1, 1000)) $ \a ->
      forAll (choose (1, 1000)) $ \b ->  
      all (`elem` factors (a*b)) [a, b]