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
-----------------------------------------------------------------------------
module Ideas.Common.Traversal.Tests 
   ( testIterator, testNavigator, tests
   , uniGen, listGen
   ) where

import Control.Monad
import Data.Maybe
import Ideas.Common.Utils.TestSuite
import Ideas.Common.Utils.Uniplate
import Ideas.Common.Traversal.Iterator
import Ideas.Common.Traversal.Navigator
import Ideas.Common.Traversal.Utils
import Test.QuickCheck

testIterator :: (Show a, Eq a, Iterator a) => String -> Gen a -> TestSuite
testIterator s gen = suite (s ++ " Iterator") $ do
   
   suite "previous/next" $ do
      prop gen "previous; next" $  hasPrevious ==>>  previous >=> next ==! id
      prop gen "next; previous" $  hasNext     ==>>  next >=> previous ==! id
      
   suite "next/final" $ do
      prop gen "isFinal"       $  isFinal . final
      prop gen "next to final" $  fixp next === final
   
   suite "previous/first" $ do
      prop gen "isFirst"           $  isFirst . first
      prop gen "previous to first" $  fixp previous === first
      
   suite "position" $ do
      prop gen "pos previous" $ 
         hasPrevious ==>> fmap position . previous ==! pred . position
      prop gen "pos next" $ 
         hasNext ==>> fmap position . next ==! succ . position
      prop gen "pos first" $ 
         (==0) . position . first
      prop gen "pos final" $
         position . final === position . fixp next
      
testNavigator :: (Show a, Eq a, Navigator a) => String -> Gen a -> TestSuite
testNavigator s gen = suite (s ++ " Navigator") $ do       
   
   suite "up/down" $ do
      prop gen "down; up"     $  hasDown ==>>      down >=> up ==! id
      prop gen "up; down"     $  hasUp   ==>>      up >=> down ==! leftMost
      prop gen "up; downLast" $  hasUp   ==>>  up >=> downLast ==! rightMost

   suite "left/right" $ do
      prop gen "right; left" $  hasRight ==>>  right >=> left ==! id
      prop gen "left; right" $  hasLeft  ==>>  left >=> right ==! id
   
   suite "up/left+right" $ do
      prop gen "left; up"  $  hasLeft  ==>>   left >=> up === up
      prop gen "right; up" $  hasRight ==>>  right >=> up === up
   
   suite "down/downLast" $ do
      prop gen "down; rightMost"       $  liftM rightMost . down === downLast
      prop gen "downLast; leftMost"    $  liftM leftMost . downLast === down
      prop gen "down is leftMost"      $  isNothing . (down >=> left)
      prop gen "downLast is rightMost" $  isNothing . (downLast >=> right)
   
   suite "location" $ do
      prop gen "loc up" $ hasUp    ==>> 
         fmap location . up ==! init . location
      prop gen "loc down" $ hasDown  ==>> 
         fmap location . down ==! (++[0]) . location
      prop gen "loc downLast" $ hasDown  ==>> 
         fmap location . downLast ==! (\a -> location a ++ [arity a-1])  
      prop gen "loc left" $ hasLeft  ==>> 
         fmap location . left ==! changeLast pred . location
      prop gen "loc right" $ hasRight ==>> 
         fmap location . right ==! changeLast succ . location
      prop gen "childnr" $ 
         childnr === fromMaybe 0 . listToMaybe . reverse . location

-------------------------------------------------------------------------
-- tests

tests :: TestSuite
tests = do 

   suite "Iterators" $ do
      testIterator "List" listGen
      testIterator "Mirror"     $ liftM makeMirror     listGen
      testIterator "Leafs"      $ liftM makeLeafs      uniGen
      testIterator "PreOrder"   $ liftM makePreOrder   uniGen
      testIterator "PostOrder"  $ liftM makePostOrder  uniGen
      testIterator "Horizontal" $ liftM makeHorizontal uniGen
      testIterator "LevelOrder" $ liftM makeLevelOrder uniGen
      
   suite "Navigators" $ do
      testNavigator "Uniplate" uniGen
      testNavigator "Mirror" $ liftM makeMirror uniGen

_go :: IO ()
_go = runTestSuiteResult tests >>= print

-------------------------------------------------------------------------
-- test utils
   
infixr 0 ===, ==!

(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(f === g) a = f a == g a

(==!) :: Eq b => (a -> Maybe b) -> (a -> b) -> a -> Bool
f ==! g = f === Just . g

infixr 0 ==>>

(==>>) :: Testable prop => (a -> Bool) -> (a -> prop) -> a -> Property
(p ==>> f) a = p a ==> f a

prop :: (Testable prop, Show a) => Gen a -> String -> (a -> prop) -> TestSuite
prop gen s = addProperty s . forAll gen

changeLast :: (a -> a) -> [a] -> [a]
changeLast _ []     = []
changeLast f [x]    = [f x]
changeLast f (x:xs) = x:changeLast f xs

data T a = T a [T a] deriving (Show, Eq)

instance Uniplate (T a) where
   uniplate (T a xs) = plate (T a) ||* xs

instance Arbitrary a => Arbitrary (T a) where
   arbitrary = sized genT
    where
      genT n = do
         a  <- arbitrary
         i  <- if n==0 then return 0 else choose (0, 5)
         xs <- vectorOf i (genT (n `div` 2))
         return (T a xs)

instance (Arbitrary a, Uniplate a) => Arbitrary (UniplateNavigator a) where
   arbitrary = liftM focus arbitrary >>= genNav
    where
      genNav a = 
         case map genNav (downs a) of
            [] -> return a
            xs -> frequency [(1, return a), (4, oneof xs)]

listGen :: Gen (ListIterator Int)
listGen = arbitrary

uniGen :: Gen (UniplateNavigator (T Int))
uniGen = arbitrary