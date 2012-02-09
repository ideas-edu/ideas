{-# LANGUAGE GADTs #-}
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
-- This module defines a type class for navigating an expression.
--
-----------------------------------------------------------------------------
module Common.Navigator
   ( -- * Type classes for navigating expressions
     IsNavigator(..), CurrentNavigator(..)
     -- * Types and constructors
   , Navigator, Location
   , navigator, noNavigator, viewNavigator, viewNavigatorWith
   , currentT, castT
     -- * Derived navigations
   , isTop, isLeaf, hasLeft, hasRight, hasPrevious, hasNext
   , downTo, navigateTo, navigateTowards, top
   ) where

import Common.Utils.Uniplate
import Common.View hiding (left, right)
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Typeable
import Common.Utils.TestSuite
import Test.QuickCheck

---------------------------------------------------------------
-- Type class for navigating expressions

type Location = [Int]

-- | For a minimal complete definition, provide an implemention for downs or
-- allDowns. All other functions need an implementation as well, except for
-- change. Note that a constructor (a -> f a) is not included in the type class
-- to allow additional type class constraints on type a.
class IsNavigator a where
   -- navigation
   up       :: a -> Maybe a
   down     :: a -> Maybe a
   downLast :: a -> Maybe a
   left     :: a -> Maybe a
   right    :: a -> Maybe a
   previous :: a -> Maybe a
   next     :: a -> Maybe a
   -- extra navigation
   allDowns :: a -> [a]
   -- inspection
   location :: a -> Location
   arity    :: a -> Int
   -- default definitions
   down       = downTo 0
   downLast a = downTo (arity a - 1) a
   arity      = length . allDowns
   left a     = join $ liftM2 downTo (liftM pred (lastLoc a)) (up a)
   right a    = join $ liftM2 downTo (liftM succ (lastLoc a)) (up a)
   previous a = liftM rec (left a) `mplus` up a
    where rec b = maybe b rec (downLast b)
   next a     = down a `mplus` rec a 
    where rec b = right b `mplus` (up b >>= rec)

class CurrentNavigator f where
   current :: f a -> a
   change  :: (a -> a) -> f a -> f a
   replace :: a -> f a -> f a
   leave   :: IsNavigator (f a) => f a -> a
   -- default definitions
   replace = change . const
   leave   = current . top

---------------------------------------------------------------
-- Derived navigations

isTop :: IsNavigator a => a -> Bool
isTop  = isNothing . up

isLeaf :: IsNavigator a => a -> Bool
isLeaf = isNothing . down

hasLeft :: IsNavigator a => a -> Bool
hasLeft = isJust . left

hasRight :: IsNavigator a => a -> Bool
hasRight = isJust . right

hasPrevious :: IsNavigator a => a -> Bool
hasPrevious = isJust . previous

hasNext :: IsNavigator a => a -> Bool
hasNext = isJust . next

hasDown a = not (isLeaf a)
hasUp a = not (isTop a)


leftMost :: IsNavigator a => a -> a
leftMost = fixp left

rightMost :: IsNavigator a => a -> a
rightMost = fixp right

leftMostLeaf :: IsNavigator a => a -> a
leftMostLeaf = fixp down

rightMostLeaf :: IsNavigator a => a -> a
rightMostLeaf = fixp downLast

fixp :: (a -> Maybe a) -> a -> a
fixp f = last . fixpl f

fixpl :: (a -> Maybe a) -> a -> [a]
fixpl f a = a : maybe [] (fixpl f) (f a)

ups :: IsNavigator a => Int -> a -> Maybe a
ups n a = foldM (const . up) a [1..n]

downs :: IsNavigator a => [Int] -> a -> Maybe a
downs is a = foldM (flip downTo) a is

lastLoc :: IsNavigator a => a -> Maybe Int
lastLoc = listToMaybe . reverse . location

downTo :: IsNavigator a => Int -> a -> Maybe a
downTo n
   | n < 0 = const Nothing
   | True  = listToMaybe . drop n . allDowns
   
navigateTo :: IsNavigator a => Location -> a -> Maybe a
navigateTo is a = ups (length js - n) a >>= downs (drop n is)
 where
   js = location a
   n  = length (takeWhile id (zipWith (==) is js))

navigateTowards :: IsNavigator a => Location -> a -> a
navigateTowards is a =
   case ups (length js - n) a of
      Just b  -> safeDowns (drop n is) b
      Nothing -> a
 where
   js = location a
   n  = length (takeWhile id (zipWith (==) is js))

   safeDowns []     b = b
   safeDowns (m:ms) b = maybe b (safeDowns ms) (downTo m b)

top :: IsNavigator a => a -> a
top a = maybe a top (up a)


data List a = Top [a]
            | Elem [a] a [a]
 deriving (Show, Eq)

instance IsNavigator (List a) where
   up (Top _)        = Nothing
   up (Elem xs a ys) = Just $ Top $ reverse xs ++ a : ys
   down (Top (x:xs)) = Just $ Elem [] x xs 
   down _ = Nothing
   downLast (Top xs) | not (null xs) = 
      case reverse xs of
         y:ys -> Just $ Elem ys y []
         _    -> Nothing
   downLast _ = Nothing
   left (Elem (x:xs) a ys) = Just $ Elem xs x (a:ys)
   left _ = Nothing
   right (Elem xs a (y:ys)) = Just $ Elem (a:xs) y ys
   right _ = Nothing
   allDowns (Top xs) = 
      let rec _ [] = []
          rec acc (y:ys) = Elem acc y ys : rec (y:acc) ys
      in rec [] xs
   allDowns _ = []
   location (Top _) = []
   location (Elem xs _ _) = [length xs]

data T a = T a [T a] deriving (Show, Eq)

root :: T a -> a
root (T a _) = a

instance Uniplate (T a) where
   uniplate (T a xs) = plate (T a) ||* xs

my :: T Int
my = T 0 [T 1 [T 2 [], T 3 [T 4 [], T 5 [], T 6 []]], T 7 [], T 8 [T 9 [], T 10 []]]

st :: UniplateNav (T Int)
st = makeUN holes my

nexts :: IsNavigator a => a -> [a]
nexts a = a : maybe [] nexts (next a)

prevs :: IsNavigator a => a -> [a]
prevs a = a : maybe [] prevs (previous a)

--prop = map (root . current) $ nexts st
back = take 50 $ map (root . current) $ prevs $ last (nexts st)

instance (Arbitrary a, Uniplate a) => Arbitrary (UniplateNav a) where
   arbitrary = liftM (makeUN holes) arbitrary >>= genNav

genNav :: IsNavigator a => a -> Gen a
genNav a = case map genNav (allDowns a) of
              [] -> return a
              xs -> frequency [(1, return a), (4, oneof xs)]

instance Arbitrary a => Arbitrary (T a) where
   arbitrary = sized genT

instance Arbitrary a => Arbitrary (List a) where
   arbitrary = liftM Top arbitrary >>= genNav

genT :: Arbitrary a => Int -> Gen (T a)
genT n = do
   a  <- arbitrary
   i  <- if n==0 then return 0 else choose (0, 5)
   xs <- vectorOf i (genT (n `div` 2))
   return (T a xs)

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

tests :: (Show a, Eq a, IsNavigator a) => Gen a -> TestSuite
tests gen = suite "navigation" $ do       
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
   prop gen "arity"    $     arity === length . allDowns
   prop gen "allDowns" $  allDowns === maybe [] (fixpl right) . down
   
go  = runTestSuite $ tests (arbitrary :: Gen (UniplateNav (T Int)))
go2 = runTestSuite $ tests (arbitrary :: Gen (List Int))

---------------------------------------------------------------
-- Instance based on Uniplate

-- The uniplate function is stored in the data type to get rid of the
-- Uniplate type class constraints in the member functions of the
-- Navigator type class.
data UniplateNav a = UN (HolesType a) [(Int, a -> a)] a

instance Eq a => Eq (UniplateNav a) where
   (==) = let f a = (current a, location a, leave a)
          in (==) `on` f

type HolesType a = a -> [(a, a -> a)]

makeUN :: HolesType a -> a -> UniplateNav a
makeUN f = UN f []

instance Show a => Show (UniplateNav a) where
   show = showNav

instance IsNavigator (UniplateNav a) where
   up (UN _ [] _)            = Nothing
   up (UN uni ((_, f):xs) a) = Just (UN uni xs (f a))

   allDowns (UN uni xs a) =
      let make i (b, f) = UN uni ((i, f):xs) b
      in zipWith make [0..] (uni a)

   location (UN _ xs _) = reverse (map fst xs)

instance CurrentNavigator UniplateNav where
   change f (UN uni xs a) = UN uni xs (f a)
   current  (UN _ _    a) = a

showNav :: (IsNavigator (f a), CurrentNavigator f, Show a) => f a -> String
showNav a = show (leave a) ++ "   { "
            ++ show (current a)
            ++ " @ " ++ show (location a) ++ " }"

---------------------------------------------------------------
-- Instance based on a View

castView :: (Typeable c, Typeable a) => View a b -> View c b
castView v = makeView f g
 where
   f e = cast e >>= matchM v
   g   = fromMaybe (error "castT: build") . cast . build v

---------------------------------------------------------------
-- Uniform navigator type

instance Show a => Show (Navigator a) where
   show = showNav

---------------------------------------------------------------
-- Constructors

navigator :: Uniplate a => a -> Navigator (Maybe a)
navigator = Simple . makeUN holes

noNavigator :: a -> Navigator (Maybe a)
noNavigator = NoNav . Just

viewNavigator :: (Uniplate a, Typeable a) => a -> Navigator (Maybe a)
viewNavigator = viewNavigatorWith holes

viewNavigatorWith :: Typeable a => HolesType a -> a -> Navigator (Maybe a)
viewNavigatorWith f = ViewNav identity . makeUN f

data Navigator a where
   ViewNav :: Typeable b => View b a -> UniplateNav b -> Navigator (Maybe a)
   Simple  :: (IsNavigator (f a), CurrentNavigator f) => f a -> Navigator (Maybe a)
   NoNav   :: a -> Navigator a
   
instance IsNavigator (Navigator a) where
   up (ViewNav v a) = liftM (ViewNav v) (up a)
   up (Simple a)    = liftM Simple (up a)
   up (NoNav _)     = Nothing
   
   allDowns (ViewNav v a) = liftM (ViewNav v) (allDowns a)
   allDowns (Simple a)    = map Simple (allDowns a)
   allDowns (NoNav _)     = []
   
   location (ViewNav _ a) = location a
   location (Simple a)    = location a
   location (NoNav _)     = []
   
instance CurrentNavigator Navigator where
   current (ViewNav v a) = matchM v (current a)
   current (Simple a)    = Just (current a)
   current (NoNav a)     = a
   
   change f (ViewNav v a) = 
      let g = simplifyWithM (f . Just) v
      in ViewNav v (change g a)
   change f (Simple a) = Simple (change (\x -> fromMaybe x (f (Just x))) a)
   change f (NoNav a)  = NoNav (f a)

currentT :: Typeable b => Navigator a -> Maybe b
currentT (Simple _)    = Nothing
currentT (NoNav _)     = Nothing
currentT (ViewNav _ a) = cast (current a)
   
castT :: Typeable e => View e b -> Navigator (Maybe a) -> Navigator (Maybe b)
castT v (ViewNav _ a) = ViewNav (castView v) a
castT _ (Simple _)    = NoNav Nothing
castT _ (NoNav _)     = NoNav Nothing