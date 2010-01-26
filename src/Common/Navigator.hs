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
     -- * Derived navigations
   , arity, isTop, isLeaf, ups, downs, navigateTo
   , top, leafs, downFirst, downLast, left, right
     -- * Instance based on Uniplate
   , Navigator, navigator
     -- * Instance based on a View
   , ViewNavigator, viewNavigator
   , changeG, currentG, unfocusG
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
   unfocus  :: Monad m => f a -> m a
   location :: f a -> [Int]
   -- adaption 
   change   :: Monad m => (a -> a)   -> f a -> m (f a)
   changeM  :: Monad m => (a -> m a) -> f a -> m (f a)
   -- default definitions
   down n a = 
      case drop n (allDowns a) of
         []   -> fail ("down " ++ show n)
         hd:_ -> return hd
   allDowns a = 
      [ fa | i <- [0 .. arity a-1], fa <- down i a ]
   change f = 
      changeM (return . f)

---------------------------------------------------------------
-- Derived navigations

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
data Navigator a = U (UniplateType a) [(Int, a -> a)] a

type UniplateType a = a -> ([a], [a] -> a)

navigator :: Uniplate a => a -> Navigator a
navigator = U uniplate []

instance Show a => Show (Navigator a) where
   show a = maybe "???" show (unfocus a) ++ "   { " 
            ++ maybe "???" show (current a) 
            ++ " @ " ++ show (location a) ++ " }"
   
instance IsNavigator Navigator where
   up (U _ [] _)            = fail "up"
   up (U uni ((_, f):xs) a) = return (U uni xs (f a))
 
   allDowns (U uni xs a) = zipWith make [0..] cs
    where
      (cs, build) = uni a
      make i = U uni ((i, build . flip (update i) cs):xs)
      update _ _ []  = []
      update i x (y:ys)
         | i == 0    = x:ys
         | otherwise = y:update (i-1) x ys
   
   location (U _ xs _) = reverse (map fst xs)
   
   changeM f (U uni xs a) = liftM (U uni xs) (f a)  
   current   (U _ _    a) = return a
   unfocus   (U _ xs a)   = return (foldl (\b (_, f) -> f b) a xs)

---------------------------------------------------------------
-- Instance based on a View

data ViewNavigator a b = VF (View a b) (Navigator a)

instance Show a => Show (ViewNavigator a b) where
   show (VF _ a) = show a
   
instance IsNavigator (ViewNavigator a) where
   up        (VF v a) = liftM (VF v) (up a)
   allDowns  (VF v a) = liftM (VF v) (allDowns a)
   location  (VF _ a) = location a
   current   (VF v a) = current a >>= matchM v
   unfocus   (VF v a) = unfocus a >>= matchM v
   changeM f (VF v a) = 
      let g b = matchM v b >>= (liftM (build v) . f) 
      in liftM (VF v) (changeM g a)

viewNavigator :: Uniplate a => View a b -> b -> ViewNavigator a b
viewNavigator v a = VF v (navigator (build v a))

changeG :: Monad m => View a c -> (c -> m c) -> ViewNavigator a b -> m (ViewNavigator a b)
changeG v f a = g a
 where g = liftM (coerce (currentView a)) . changeM f . coerce v

currentG :: Monad m => View a c -> ViewNavigator a b -> m c
currentG v = current . coerce v

unfocusG :: Monad m => View a c -> ViewNavigator a b -> m c
unfocusG v = unfocus . coerce v

coerce :: View a c -> ViewNavigator a b -> ViewNavigator a c
coerce v (VF _ a) = VF v a

currentView :: ViewNavigator a b -> View a b
currentView (VF v _) = v





{-

uniN :: Uniplate a => a -> N a
uniN a = N (makeU a)

--exprN :: Expr -> N Expr
--exprN a = N (exprFocus a)

noN :: a -> N a
noN a = N (noFocus a)
   
data N a = forall f . IsNavigator f => N (f a)

instance IsNavigator N where
   up        (N a) = liftM N (up a)
   allDowns  (N a) = map N (allDowns a)
   location  (N a) = location a
   changeM f (N a) = liftM N (changeM f a)
   current   (N a) = current a
   unfocus   (N a) = unfocus a

-------------------------------------------------

newtype NoFocus a = NF a

instance Show a => Show (NoFocus a) where
   show (NF a) = show a
   
instance IsNavigator NoFocus where
   up          = fail "up for nofocus" 
   allDowns _  = []
   location _  = []
   current     = return . fromNoFocus
   unfocus     = return . fromNoFocus
   changeM f a = current a >>= liftM noFocus . f
      
noFocus :: a -> NoFocus a
noFocus = NF

fromNoFocus :: NoFocus a -> a
fromNoFocus (NF a) = a -}