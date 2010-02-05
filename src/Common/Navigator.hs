{-# OPTIONS -XExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
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
   ( -- * Type class for navigating expressions 
     IsNavigator(..)
     -- * Types and constructors 
   , Navigator, NavigatorG
   , navigator, navigatorG, noNavigator, useView
     -- * Derived navigations
   , leave, replace, arity, isTop, isLeaf, ups, downs, navigateTo
   , top, leafs, downFirst, downLast, left, right
     -- * Generalized functions
   , changeG, currentG, leaveG
   ) where

import Common.Uniplate
import Common.View hiding (left, right)
import Control.Monad
import Data.Maybe

---------------------------------------------------------------
-- Type class for navigating expressions

-- | For a minimal complete definition, provide an implemention for downs or
-- allDowns. All other functions need an implementation as well, except for 
-- change. Note that a constructor (a -> f a) is not included in the type class
-- to allow additional type class constraints on type a.
class IsNavigator f where
   -- navigation
   up       :: Monad m => f a -> m (f a)
   down     :: Monad m => Int -> f a -> m (f a)
   allDowns :: f a -> [f a]
   -- inspection
   current  :: Monad m => f a -> m a
   location :: f a -> [Int]
   -- adaption 
   change   :: (a -> a) -> f a -> f a
   changeM  :: Monad m => (a -> m a) -> f a -> m (f a)
   -- default definitions
   down n a = 
      case drop n (allDowns a) of
         []   -> fail ("down " ++ show n)
         hd:_ -> return hd
   allDowns a = 
      [ fa | i <- [0 .. arity a-1], fa <- down i a ]
   change f a =
      case changeM (Just . f) a of
         Just new -> new
         Nothing  -> a

---------------------------------------------------------------
-- Derived navigations

leave  :: (IsNavigator f, Monad m) => f a -> m a
leave a = maybe (current a) leave (up a)

replace :: IsNavigator f => a -> f a -> f a
replace = change . const

arity :: IsNavigator f => f a -> Int
arity  = length . allDowns

isTop :: IsNavigator f => f a -> Bool
isTop  = isNothing . up

isLeaf :: IsNavigator f => f a -> Bool
isLeaf = null . allDowns

ups :: (IsNavigator f, Monad m) => Int -> f a -> m (f a)
ups n a = foldM (const . up) a [1..n]

downs :: (IsNavigator f, Monad m) => [Int] -> f a -> m (f a)
downs is a = foldM (flip down) a is

navigateTo :: (IsNavigator f, Monad m) => [Int] -> f a -> m (f a)
navigateTo is a = ups (length js - n) a >>= downs (drop n is)
 where 
   js = location a
   n  = length (takeWhile id (zipWith (==) is js))

top :: (IsNavigator f, Monad m) => f a -> m (f a)
top = navigateTo []

leafs :: IsNavigator f => f a -> [f a]
leafs a 
   | isLeaf a  = [a]
   | otherwise = concatMap leafs (allDowns a)

downFirst :: (IsNavigator f, Monad m) => f a -> m (f a)
downFirst = down 0

downLast :: (IsNavigator f, Monad m) => f a -> m (f a)
downLast a = down (arity a - 1) a

left :: (IsNavigator f, Monad m) => f a -> m (f a)
left a0 = rec a0
 where 
   rec a
      | isTop a   = downFirst a0
      | i == 0    = up a >>= rec
      | otherwise = up a >>= down (i-1)
    where
      i = last (location a)
 
right :: (IsNavigator f, Monad m) => f a -> m (f a)
right a0 = rec a0
 where 
   rec a
      | isTop a   = downLast a0
      | otherwise = do
           p <- up a
           let n = arity p
           if i >= n-1 then rec p else down (i+1) p 
    where 
      i = last (location a)  

---------------------------------------------------------------
-- Instance based on Uniplate

-- The uniplate function is stored in the data type to get rid of the
-- Uniplate type class constraints in the member functions of the 
-- Navigator type class.
data UniplateNav a = UN (UniplateType a) [(Int, a -> a)] a

type UniplateType a = a -> ([a], [a] -> a)

makeUN :: Uniplate a => a -> UniplateNav a
makeUN = UN uniplate []

instance Show a => Show (UniplateNav a) where
   show a = maybe "???" show (leave a) ++ "   { " 
            ++ maybe "???" show (current a) 
            ++ " @ " ++ show (location a) ++ " }"
   
instance IsNavigator UniplateNav where
   up (UN _ [] _)            = fail "up"
   up (UN uni ((_, f):xs) a) = return (UN uni xs (f a))
 
   allDowns (UN uni xs a) = zipWith make [0..] cs
    where
      (cs, build) = uni a
      make i = UN uni ((i, build . flip (update i) cs):xs)
      update _ _ []  = []
      update i x (y:ys)
         | i == 0    = x:ys
         | otherwise = y:update (i-1) x ys
   
   location (UN _ xs _) = reverse (map fst xs)
   
   changeM f (UN uni xs a) = liftM (UN uni xs) (f a)  
   current   (UN _ _    a) = return a

---------------------------------------------------------------
-- Instance based on a View

type Navigator  a   = NavigatorG a a
data NavigatorG a b = NG (View a b) (UniplateNav a)

instance Show a => Show (NavigatorG a b) where
   show (NG _ a) = show a
   
instance IsNavigator (NavigatorG a) where
   up        (NG v a) = liftM (NG v) (up a)
   allDowns  (NG v a) = liftM (NG v) (allDowns a)
   location  (NG _ a) = location a
   current   (NG v a) = current a >>= matchM v
   changeM f (NG v a) = 
      let g b = matchM v b >>= (liftM (build v) . f) 
      in liftM (NG v) (changeM g a)

coerce :: View a c -> NavigatorG a b -> NavigatorG a c
coerce v (NG _ a) = NG v a

currentView :: NavigatorG a b -> View a b
currentView (NG v _) = v
   
---------------------------------------------------------------
-- Constructors

navigator :: Uniplate a => a -> Navigator a
navigator = NG identity . makeUN

navigatorG :: Uniplate a => View a b -> b -> NavigatorG a b
navigatorG v b = NG v (makeUN (build v b))

noNavigator :: a -> Navigator a
noNavigator = NG identity . UN (\a -> ([], const a)) []

useView :: View a b -> Navigator a -> NavigatorG a b
useView = coerce

---------------------------------------------------------------
-- Generalized functions

changeG :: Monad m => View a c -> (c -> m c) -> NavigatorG a b -> m (NavigatorG a b)
changeG v f a = g a
 where g = liftM (coerce (currentView a)) . changeM f . coerce v

currentG :: Monad m => View a c -> NavigatorG a b -> m c
currentG v = current . coerce v

leaveG :: Monad m => View a c -> NavigatorG a b -> m c
leaveG v = leave . coerce v