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
module Common.Traversal.Tests where

import Control.Monad
import Data.Maybe
import Common.Utils.TestSuite
import Common.Utils.Uniplate
import Common.Traversal.Iterator
import Common.Traversal.Navigator
import Common.Traversal.Utils
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
      
testNavigator :: (Show a, Eq a, Navigator a) => Gen a -> TestSuite
testNavigator gen = suite "navigation" $ do       
   -- up/down
   prop gen "down ; up"     $  hasDown ==>>      down >=> up ==! id
   prop gen "up ; down"     $  hasUp   ==>>      up >=> down ==! leftMost
   prop gen "up ; downLast" $  hasUp   ==>>  up >=> downLast ==! rightMost

   -- left/right
   prop gen "right ; left" $  hasRight ==>>  right >=> left ==! id
   prop gen "left ; right" $  hasLeft  ==>>  left >=> right ==! id
   
   -- up/left+right
   prop gen "left ; up"  $  hasLeft  ==>>   left >=> up === up
   prop gen "right ; up" $  hasRight ==>>  right >=> up === up
   
   -- down/downLast
   prop gen "down ; right*"         $  liftM rightMost . down === downLast
   prop gen "downLast ; left*"      $  liftM leftMost . downLast === down
   prop gen "down is leftMost"      $  isNothing . (down >=> left)
   prop gen "downLast is rightMost" $  isNothing . (downLast >=> right)
   
   -- special elements
   prop gen "top"           $  isTop  . top
   prop gen "leftMostLeaf"  $  isLeaf . leftMostLeaf
   prop gen "rightMostLeaf" $  isLeaf . rightMostLeaf
   
   -- remaining
   prop gen "location" $  (\a -> navigateTo (location a) (top a)) ==! id
   prop gen "arity"    $     arity === length . downs
   prop gen "allDowns" $  downs === maybe [] (fixpl right) . down
   
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

go, go1, go2, go3, go4, go5, go6, goX, goY :: IO ()
go  = runTestSuite $ testIterator "List" listGen
go1 = runTestSuite $ testIterator "Leafs"      $ liftM makeLeafs uniGen
go2 = runTestSuite $ testIterator "PreOrder"   $ liftM makePreOrder uniGen
go3 = runTestSuite $ testIterator "Mirror"     $ liftM makeMirror listGen
go4 = runTestSuite $ testIterator "PostOrder"  $ liftM makePostOrder uniGen
go5 = runTestSuite $ testIterator "Horizontal" $ liftM makeHorizontal uniGen
go6 = runTestSuite $ testIterator "LevelOrder" $ liftM makeLevelOrder uniGen



goX = runTestSuite $ testNavigator uniGen
goY = runTestSuite $ testNavigator $ liftM makeMirror uniGen