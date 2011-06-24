{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- Decimal fractions: the denominator of such a fraction must a power of 10.
-- Division in the Fractional type class is not safe.
--
-----------------------------------------------------------------------------
module Domain.Math.Data.DecimalFraction 
   ( DecimalFraction, divide, power, fromDouble, validDivisor, digits
   ) where

import Control.Monad
import Data.Maybe
import Data.Ratio

-- |Data type for decimal fractions
newtype DecimalFraction = DF Rational -- Invariant: denominator is valid
   deriving (Eq, Ord, Num, Real)
   
instance Show DecimalFraction where
   show d@(DF r) = show x ++ "." ++ replicate extra '0' ++ show y
    where
      digs   = digits d
      base   = 10^digs
      n      = numerator (r * fromInteger base)
      (x, y) = n `divMod` base
      extra  = digs - length (show y)   

instance Fractional DecimalFraction where
   a/b = fromMaybe (error "invalid divisor") (divide a b)
   fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- |Safe division of two decimal fractions
divide :: DecimalFraction -> DecimalFraction -> Maybe DecimalFraction
divide (DF a) (DF b) = do
   guard (validDivisor (DF b))
   return (DF (a/b))

-- |Safe power of two decimal fractions
power :: DecimalFraction -> DecimalFraction -> Maybe DecimalFraction
power x (DF r)
   | denominator r /= 1 = Nothing
   | y >= 0             = Just a
   | otherwise          = divide 1 a
 where
   y = numerator r
   a = x Prelude.^ abs y

-- | Approximation of a double, with a precision of 8 digits
fromDouble :: Double -> DecimalFraction
fromDouble d = DF (fromInteger base / 10^digs)
 where
   digs = 8 :: Int -- maximum number of digits
   base = round (d * 10^digs) :: Integer

-- |Tests whether it is safe to divide by this fraction: it is safe to divide 
-- if its numerator(!) is a product of two's and five's.
validDivisor :: DecimalFraction -> Bool
validDivisor (DF a) = validDenominator (abs (numerator a))

-- |number of decimal digits
digits :: DecimalFraction -> Int
digits (DF r) = head $ filter p [0..]
 where
   p i = 10^i `mod` denominator r == 0

-- local helper
validDenominator :: Integer -> Bool
validDenominator n
   | even n         = validDenominator (n `div` 2)
   | n `mod` 5 == 0 = validDenominator (n `div` 5)
   | otherwise      = n == 1