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
-- Algebraic laws, for testing purposes
--
-----------------------------------------------------------------------------
module Domain.Math.Numeric.Laws 
   ( numLaws, testNumLaws, testNumLawsWith
   , fracLaws, testFracLaws, testFracLawsWith
   ) where

import Common.TestSuite
import Test.QuickCheck

testNumLaws :: Num a => String -> Gen a -> TestSuite
testNumLaws = testNumLawsWith (==)

testNumLawsWith :: Num a => (a -> a -> Bool) -> String -> Gen a -> TestSuite
testNumLawsWith eq s g = suite ("Num instance for " ++ s) $
   mapM_ ($ g) (numLaws eq)

testFracLaws :: Fractional a => String -> Gen a -> TestSuite
testFracLaws = testFracLawsWith (==)

testFracLawsWith :: Fractional a => (a -> a -> Bool) -> String -> Gen a -> TestSuite
testFracLawsWith eq s g = suite ("Fractional instance for " ++ s) $ 
   mapM_ ($ g) (fracLaws eq)

numLaws :: Num a => (a -> a -> Bool) -> [Gen a -> TestSuite]
numLaws eq =
   [ law1 "plus zero left"     $ \a      ->      0+a == a
   , law1 "plus zero right"    $ \a      ->      a+0 == a
   , law2 "plus comm"          $ \a b    ->      a+b == b+a
   , law3 "plus trans"         $ \a b c  ->  a+(b+c) == (a+b)+c
   , law1 "negate zero"        $ \a      ->       -0 == 0        `asTypeOf` a
   , law1 "negate double"      $ \a      ->    -(-a) == a
   , law1 "minus zero left"    $ \a      ->      0-a == -a
   , law1 "minus zero right"   $ \a      ->      a-0 == a
   , law2 "negate plus"        $ \a b    ->   -(a+b) == -a-b
   , law2 "negate minus"       $ \a b    ->   -(a-b) == -a+b
   , law2 "plus negate"        $ \a b    ->   a+(-b) == a-b
   , law1 "times zero left"    $ \a      ->      0*a == 0
   , law1 "times zero right"   $ \a      ->      a*0 == 0
   , law1 "times one left"     $ \a      ->      1*a == a
   , law1 "times one right"    $ \a      ->      a*1 == a
   , law2 "times comm"         $ \a b    ->      a*b == b*a
   , law3 "times trans"        $ \a b c  ->  a*(b*c) == (a*b)*c
   , law2 "times negate left"  $ \a b    ->   (-a)*b == -(a*b)
   , law2 "times negate right" $ \a b    ->   a*(-b) == -(a*b)
   , law3 "times plus left"    $ \a b c  ->  (a+b)*c == a*c + b*c
   , law3 "times plus right"   $ \a b c  ->  a*(b+c) == a*b + a*c
   , law3 "times minus left"   $ \a b c  ->  (a-b)*c == a*c - b*c
   , law3 "times minus right"  $ \a b c  ->  a*(b-c) == a*b - a*c
   ]
 where
   infix 4 ==
   a == b = property (a `eq` b)

fracLaws :: Fractional a => (a -> a -> Bool) -> [Gen a -> TestSuite]
fracLaws eq =
   [ law3 "division numerator"   $ \a b c  ->      (a/b)/c == a/(b*c)          <| b/=0 && c/=0
   , law3 "division denominator" $ \a b c  ->      a/(b/c) == a*(c/b)          <| b/=0 && c/=0
   , law1 "zero numerator"       $ \a      ->          0/a == 0 <| a/=0
   , law1 "one numerator"        $ \a      ->          1/a == recip a          <| a/=0
   , law1 "one denominator"      $ \a      ->          a/1 == a
   , law1 "division is one"      $ \a      ->          a/a == 1                <| a/=0
   , law1 "recip double"         $ \a      ->            a == recip (recip a)  <| a/=0
   , law3 "times division left"  $ \a b c  ->      (a/b)*c == (a*c)/b          <| b/=0
   , law3 "times division right" $ \a b c  ->      a*(b/c) == (a*b)/c          <| c/=0
   , law3 "plus division left"   $ \a b c  ->      (a/b)+c == (a+c*b)/b        <| b/=0
   , law3 "plus division right"  $ \a b c  ->      a+(b/c) == (a*c+b)/c        <| c/=0
   , law3 "minus division left"  $ \a b c  ->      (a/b)-c == (a-c*b)/b        <| b/=0
   , law3 "minus division right" $ \a b c  ->      a-(b/c) == (a*c-b)/c        <| c/=0
   , law2 "negate numerator"     $ \a b    ->      a/(-b)  == -(a/b)           <| b/=0
   , law2 "negate denominator"   $ \a b    ->       (-a)/b == -(a/b)           <| b/=0
   , law2 "recip times"          $ \a b    ->  recip (a*b) == recip a*recip b  <| a/=0 && b/=0
   , law2 "recip division"       $ \a b    ->  recip (a/b) == b/a              <| a/=0 && b/=0
   ]
 where
   infix 4 ==
   a == b = property (a `eq` b)
   infix 1 <|
   p <| b = b ==> p

-- local helper-functions
law1 :: Show a => String -> (a -> Property) -> Gen a -> TestSuite
law1 s p g = addProperty s (make g id p)

law2 :: Show a => String -> (a -> a -> Property) -> Gen a -> TestSuite
law2 s p g = addProperty s (make g (make g id) p)

law3 :: Show a => String -> (a -> a -> a -> Property) -> Gen a -> TestSuite
law3 s p g = addProperty s (make g (make g (make g id)) p)

make g c p = forAll g (c . p)