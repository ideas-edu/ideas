{-# OPTIONS -fglasgow-exts #-}

module Domain.Fraction.Generator 
   ( generateFrac, generateFracWith
   , FracGenConfig(..), defaultConfig
   ) where

import Domain.Fraction.Frac
import Control.Monad
import Data.Char
import Test.QuickCheck hiding (defaultConfig)
import System.Random
import Ratio

generateFrac :: Gen Frac
generateFrac = generateFracWith defaultConfig
   
generateFracWith :: FracGenConfig -> Gen Frac
generateFracWith = arbFracNZ
   
data FracGenConfig = FracGenConfig
   { maxSize       :: Int
   , differentVars :: Int
   , freqConstant  :: Int
   , freqVariable  :: Int
   , freqMul       :: Int
   , freqDiv       :: Int
   , freqAdd       :: Int
   , freqSub       :: Int
   }
 deriving Show

defaultConfig :: FracGenConfig
defaultConfig = FracGenConfig
   { maxSize       = 3
   , differentVars = 2
   , freqConstant  = 4
   , freqVariable  = 2
   , freqMul       = 2
   , freqDiv       = 2
   , freqAdd       = 3
   , freqSub       = 3
   }

freqLeaf :: FracGenConfig -> Int
freqLeaf config = freqConstant config + freqVariable config

arbFrac :: FracGenConfig -> Gen Frac
arbFrac config 
   | maxSize config == 0 = oneof [liftM fromRational arbitrary, return $ Var "x"]
   | otherwise           = oneof [ arbFrac config {maxSize = 0}, liftM2 (:+:) rec rec, liftM2 (:*:) rec rec
                                 , liftM2 (:/:) rec recNZ, liftM2 (:-:) rec rec
                                 ]
                         where 
                           rec   = arbFrac config {maxSize = (n `div` 2)}
                           recNZ = arbFracNZ config {maxSize = (n `div` 2)}
                           n     = maxSize config

-- non-zero value
arbFracNZ :: FracGenConfig -> Gen Frac
arbFracNZ config
   | maxSize config == 0 = oneof [liftM fromRational arbRatioNZ, return $ Var "x"]
   | otherwise           = oneof [ arbFracNZ config {maxSize = 0}, liftM2 (:*:) recNZ recNZ
                                 , liftM2 (:/:) recNZ recNZ, liftM2 (:+:) recNZ recNZ 
                                 , liftM2 (:-:) recNZ recNZ
                                 ]
                         where
--                           rec   = arbFrac config {maxSize = (n `div` 2)}
                           recNZ = arbFracNZ config {maxSize = (n `div` 2)}
                           n     = maxSize config

arbRatioNZ :: Gen Rational
arbRatioNZ = do
   n' <- arbitrary
   d' <- arbitrary
   let d = if d' == 0 then 1 else d'
   let n = if n' == 0 then 1 else n'
   return (n % d)

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary Frac where
   arbitrary = sized $ \n -> arbFrac defaultConfig {maxSize = n}
   coarbitrary (Var x)       = variant 0 . coarbitrary (map ord x)
   coarbitrary (Lit x)       = variant 1 . coarbitrary x
   coarbitrary (x :*: y)     = variant 1 . coarbitrary x . coarbitrary y
   coarbitrary (x :/: y)     = variant 2 . coarbitrary x . coarbitrary y
   coarbitrary (x :+: y)     = variant 3 . coarbitrary x . coarbitrary y
   coarbitrary (x :-: y)     = variant 4 . coarbitrary x . coarbitrary y


instance Arbitrary Rational where
    arbitrary = do
        n <- arbitrary
        d' <- arbitrary
        let d = if d' == 0 then 1 else d'
        return (n % d)
    coarbitrary = undefined

