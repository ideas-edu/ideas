{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
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

import Common.Rewriting
import Domain.Fraction.Frac
import Control.Monad
import Data.Char
import Test.QuickCheck hiding (defaultConfig)

instance Rewrite Frac

generateFrac :: Gen Frac
generateFrac = generateFracWith defaultConfig
   
generateFracWith :: FracGenConfig -> Gen Frac
generateFracWith = arbFrac
   
data FracGenConfig = FracGenConfig
   { maxSize    :: Int
   , maxInt     :: Integer
   , diffVars   :: Int
   , freqConst  :: Int
   , freqVar    :: Int
   , freqMul    :: Int
   , freqDiv    :: Int
   , freqAdd    :: Int
   , freqSub    :: Int
   , freqNeg    :: Int
   }
 deriving Show

defaultConfig :: FracGenConfig
defaultConfig = FracGenConfig
   { maxSize   = 2
   , maxInt    = 6
   , diffVars  = 2
   , freqConst = 0
   , freqVar   = 1
   , freqMul   = 2
   , freqDiv   = 2
   , freqAdd   = 3
   , freqSub   = 3
   , freqNeg   = 3
   }

-- decrease size and frequencies
decConfig :: FracGenConfig -> FracGenConfig
decConfig cfg = cfg
   { maxSize    = (maxSize cfg) `div` 2
   , freqConst  = dec (freqConst  cfg)
   , freqVar    = dec (freqVar    cfg)
   , freqMul    = dec (freqMul    cfg)
   , freqDiv    = dec (freqDiv    cfg)
   , freqAdd    = dec (freqAdd    cfg)
   , freqSub    = dec (freqSub    cfg)
   , freqNeg    = dec (freqNeg    cfg)
   }
   where dec i | i>0       = i-1
               | otherwise = i

-- list of variables
varList :: FracGenConfig -> [Gen Frac]
varList cfg = take (diffVars cfg) $ map (return.Var.(:[])) ['x'..]    

-- restrain the generated integers
ints, posints :: (Num a) => FracGenConfig -> Gen a
ints    cfg = liftM fromInteger $ choose (0, maxInt cfg)
posints cfg = liftM fromInteger $ choose (1, maxInt cfg)

-- make use of the frequencies
freqConfig :: FracGenConfig -> [(Int, Gen Frac)]
freqConfig cfg =
   [ (freqConst cfg, ints cfg)
   , (freqVar cfg,   oneof (varList cfg))
   , (freqMul cfg,   liftM2 (:*:) rec nv)
   , (freqDiv cfg,   liftM2 (:/:) rec const)
   , (freqAdd cfg,   liftM2 (:+:) rec rec)
   , (freqSub cfg,   liftM2 (:-:) rec rec)
   , (freqNeg cfg,   liftM Neg $ rec)
   ]
   where rec   = arbFrac cfg'
         nv    = arbFrac cfg' {freqVar = 0}  -- no variables
         const = arbFrac cfg' {freqVar = 0, freqSub = 0, freqNeg = 0} -- no vars and non-zero
         cfg'  = decConfig cfg


arbFrac :: FracGenConfig -> Gen Frac
arbFrac config 
   | maxSize config == 0 =  liftM2 (:/:) (ints config) (posints config)
   | otherwise           =  frequency (freqConfig config) 

-----------------------------------------------------------
--- QuickCheck generator

instance Arbitrary Frac where
   arbitrary = sized $ \n -> arbFrac defaultConfig {maxSize = n}
   coarbitrary frac = 
     case frac of 
       Var x   -> variant 0 . coarbitrary (map ord x)
       Con x   -> variant 1 . coarbitrary x
       x :*: y -> variant 1 . coarbitrary x . coarbitrary y
       x :/: y -> variant 2 . coarbitrary x . coarbitrary y
       x :+: y -> variant 3 . coarbitrary x . coarbitrary y
       x :-: y -> variant 4 . coarbitrary x . coarbitrary y
       Neg x   -> variant 5 . coarbitrary x
