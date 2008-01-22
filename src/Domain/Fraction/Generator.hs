{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
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
   { maxSize       = 4
   , differentVars = 2
   , freqConstant  = 4
   , freqVariable  = 1
   , freqMul       = 2
   , freqDiv       = 2
   , freqAdd       = 3
   , freqSub       = 3
   }

freqLeaf :: FracGenConfig -> Int
freqLeaf config = freqConstant config + freqVariable config


-- Needs to be redesigned to take freq. variables into account

-- no variables
arbFracNoVars :: FracGenConfig -> Gen Frac
arbFracNoVars config 
   | maxSize config == 0 = liftM fromInteger arbitrary
   | otherwise           = oneof [ arbFracNoVars config {maxSize = 0}
                                 , liftM2 (:+:) rec rec
                                 , liftM2 (:*:) rec rec
                                 , liftM2 (:/:) rec nz
                                 , liftM2 (:-:) rec rec
                                 ]
                         where 
                           rec   = arbFracNoVars config {maxSize = (n `div` 2)}
                           nz    = arbFracNoVarsNZ config {maxSize = (n `div` 2)}
                           n     = maxSize config

-- non zero and non var value
arbFracNoVarsNZ :: FracGenConfig -> Gen Frac
arbFracNoVarsNZ config 
   | maxSize config == 0 = liftM fromInteger arbIntNZ
   | otherwise           = oneof [ arbFracNoVarsNZ config {maxSize = 0}
                                 , liftM2 (:+:) rec rec
                                 , liftM2 (:*:) rec rec
                                 , liftM2 (:/:) rec rec
                                 ]
                         where 
                           rec   = arbFracNoVarsNZ config {maxSize = (n `div` 2)}
                           n     = maxSize config

arbFrac :: FracGenConfig -> Gen Frac
arbFrac config 
   | maxSize config == 0 = liftM fromInteger arbitrary
   | otherwise           = oneof [ arbFrac config {maxSize = 0}
                                 , liftM2 (:+:) rec rec
                                 , liftM2 (:*:) rec nv  -- no higher order
                                 , liftM2 (:/:) rec const
                                 , liftM2 (:-:) rec rec
                                 , return $ Var "x"
                                 ]
                         where 
                           rec   = arbFrac config {maxSize = (n `div` 2)}
                           nz    = arbFracNZ config {maxSize = (n `div` 2)}
                           nv    = arbFracNoVars config {maxSize = (n `div` 2)}
                           n     = maxSize config
                           const = arbFracNoVarsNZ config {maxSize = (n `div` 2)}

-- non-zero value
arbFracNZ :: FracGenConfig -> Gen Frac
arbFracNZ config
   | maxSize config == 0 = liftM fromInteger arbIntNZ
   | otherwise           = oneof [ arbFracNZ config {maxSize = 0}
                                 , liftM2 (:*:) const nz
                                 , liftM2 (:/:) nz const
                                 , liftM2 (:+:) nz nz 
                                 , return $ Var "x"
                                 ]
                         where
                           nz    = arbFracNZ config {maxSize = (n `div` 2)}
                           n     = maxSize config
                           const = arbFracNoVarsNZ config {maxSize = (n `div` 2)}

arbIntNZ :: Gen Integer
arbIntNZ = do
   x' <- arbitrary
   let x = if x' == 0 then 1 else x'
   return x

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary Frac where
   arbitrary = sized $ \n -> arbFrac defaultConfig {maxSize = n}
   coarbitrary (Var x)       = variant 0 . coarbitrary (map ord x)
   coarbitrary (Con x)       = variant 1 . coarbitrary x
   coarbitrary (x :*: y)     = variant 1 . coarbitrary x . coarbitrary y
   coarbitrary (x :/: y)     = variant 2 . coarbitrary x . coarbitrary y
   coarbitrary (x :+: y)     = variant 3 . coarbitrary x . coarbitrary y
   coarbitrary (x :-: y)     = variant 4 . coarbitrary x . coarbitrary y

