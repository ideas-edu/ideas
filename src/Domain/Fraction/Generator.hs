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
generateFracWith = arbFrac
   
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
   { maxSize       = 2
   , differentVars = 3
   , freqConstant  = 1
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
   | maxSize config == 0 = arbFracLeaf config
   | otherwise           = arbFracBin  config

arbFracLeaf :: FracGenConfig -> Gen Frac
arbFracLeaf config = frequency
   [ (freqConstant config, oneof [liftM fromRational arbitrary])
   , (freqVariable config, oneof [ return (Var x) | x <- take (differentVars config) variableList])
   ]

arbFracBin :: FracGenConfig -> Gen Frac
arbFracBin config = frequency
   [ (freqLeaf config, arbFracLeaf config)
   , (freqMul  config, op2 (:*:))
   , (freqDiv  config, op2 (:/:))
   , (freqAdd  config, op2 (:+:))
   , (freqSub  config, op2 (:-:))
   ]
 where
   rec   = arbFrac config {maxSize = maxSize config `div` 2}
--   op1 f = liftM  f rec
   op2 f = liftM2 f rec rec

variableList :: [String]
variableList = ["x", "y", "z"] ++ [ "x" ++ show n | n <- [0..] ]

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary Frac where
   arbitrary = sized $ \n -> arbFrac defaultConfig {maxSize = n}
   coarbitrary frac = 
      case frac of
         Var x    -> variant 0 . coarbitrary (map ord x)
         Lit x    -> variant 1 . coarbitrary x
         x :*: y  -> variant 1 . coarbitrary x . coarbitrary y
         x :/: y  -> variant 2 . coarbitrary x . coarbitrary y
         x :+: y  -> variant 3 . coarbitrary x . coarbitrary y
         x :-: y  -> variant 4 . coarbitrary x . coarbitrary y

instance Arbitrary Rational where
    arbitrary = do
        n <- arbitrary
        d' <- arbitrary
        let d =  if d' == 0 then 1 else d'
        return (n % d)
    coarbitrary = undefined
