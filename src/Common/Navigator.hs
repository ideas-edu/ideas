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
   ( -- * Type classes for navigating expressions 
     IsNavigator(..), TypedNavigator(..)
     -- * Types and constructors 
   , Navigator, Location
   , navigator, noNavigator, viewNavigator
     -- * Derived navigations
   , leave, replace, arity, isTop, isLeaf, ups, downs, navigateTo
   , top, leafs, downFirst, downLast, left, right
   ) where

import Common.Uniplate
import Common.View hiding (left, right)
import Control.Monad
import Data.Maybe
import Data.Typeable

---------------------------------------------------------------
-- Type class for navigating expressions

type Location = [Int]

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
   location :: f a -> Location
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

class IsNavigator f => TypedNavigator f where
   changeT  :: (Monad m, Typeable b) => (b -> m b) -> f a -> m (f a) 
   currentT :: (Monad m, Typeable b) => f a -> m b
   leaveT   :: (Monad m, Typeable b) => f a -> m b
   castT    :: (Monad m, Typeable e) => View e b -> f a -> m (f b)
   -- By default, fail
   changeT _ _ = fail "changeT"
   currentT _  = fail "currentT"
   leaveT _    = fail "leaveT"
   castT _ _   = fail "castT"

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

navigateTo :: (IsNavigator f, Monad m) => Location -> f a -> m (f a)
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
   show = showNav
   
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

showNav :: (IsNavigator f, Show a) => f a -> String
showNav a = maybe "???" show (leave a) ++ "   { " 
            ++ maybe "???" show (current a) 
            ++ " @ " ++ show (location a) ++ " }"

---------------------------------------------------------------
-- Instance based on a View

data ViewNav a b = VN (View a b) (UniplateNav a)

instance Show a => Show (ViewNav a b) where
   show (VN _ a) = show a
   
instance IsNavigator (ViewNav a) where
   up        (VN v a) = liftM (VN v) (up a)
   allDowns  (VN v a) = liftM (VN v) (allDowns a)
   location  (VN _ a) = location a
   current   (VN v a) = current a >>= matchM v
   changeM f (VN v a) = 
      let g b = matchM v b >>= (liftM (build v) . f) 
      in liftM (VN v) (changeM g a)

instance Typeable a => TypedNavigator (ViewNav a) where
   changeT f (VN v a) = do
      new <- current a >>= castM >>= f >>= castM
      return (VN v (replace new a))
   currentT (VN _ a) = 
      current a >>= castM
   leaveT (VN _ a) =
      leave a >>= castM
   castT v (VN v0 a) 
      | typeOf (getTp v) == typeOf (getTp v0) = 
           return (VN (makeView f g) a)
      | otherwise = 
           fail "castT"
    where
      f e = castM e >>= matchM v
      g   = fromMaybe (error "castT") . cast . build v
      
      getTp :: View a b -> a
      getTp = error "castT"

castM :: (Monad m, Typeable a, Typeable b) => a -> m b
castM = maybe (fail "castM") return . cast

---------------------------------------------------------------
-- Uniform navigator type

instance Show a => Show (Navigator a) where
   show = showNav

data Navigator a = forall f . TypedNavigator f => N (f a)
data Simple    a = forall f . IsNavigator f    => S (f a)

instance IsNavigator Navigator where
   up        (N a) = liftM N (up a)
   allDowns  (N a) = map N (allDowns a)
   current   (N a) = current a
   location  (N a) = location a
   changeM f (N a) = liftM N (changeM f a)

instance TypedNavigator Navigator where
   changeT f (N a) = liftM N (changeT f a)
   currentT  (N a) = currentT a
   leaveT    (N a) = leaveT a
   castT v   (N a) = liftM N (castT v a)

instance IsNavigator Simple where
   up        (S a) = liftM S (up a)
   allDowns  (S a) = map S (allDowns a)
   current   (S a) = current a
   location  (S a) = location a
   changeM f (S a) = liftM S (changeM f a)

instance TypedNavigator Simple

---------------------------------------------------------------
-- Constructors

navigator :: Uniplate a => a -> Navigator a
navigator = N . S . makeUN

noNavigator :: a -> Navigator a
noNavigator = N . S . UN (\a -> ([], const a)) []

viewNavigator :: (Uniplate a, Typeable a) => a -> Navigator a
viewNavigator = N . VN identity . makeUN