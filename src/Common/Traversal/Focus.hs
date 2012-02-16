{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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
module Common.Traversal.Focus
   ( Update(..), Focus(..)
   , current, change, replace
   , UniplateNavigator, uniplateNavigator
   , ListIterator, StrIterator
   ) where

import Common.Traversal.Utils
import Common.Traversal.Iterator
import Common.Traversal.Navigator
import Common.Utils.Uniplate
import Control.Monad
import Data.List
import Data.Maybe
import Data.Generics.Str

----------------------------------------------------------------
-- Tree walks

newtype Mirror a = Mirror a

mirrored :: (Mirror a -> Maybe (Mirror a)) -> a -> Maybe a
mirrored = unliftWrapper

instance Iterator a => Iterator (Mirror a) where
   next     (Mirror a) = liftM Mirror (previous a)
   previous (Mirror a) = liftM Mirror (next a)
   first    (Mirror a) = Mirror (final a)
   final    (Mirror a) = Mirror (first a)

instance Navigator a => Navigator (Mirror a) where
   up       (Mirror a) = liftM Mirror (up a)
   down     (Mirror a) = liftM Mirror (downLast a)
   downLast (Mirror a) = liftM Mirror (down a)
   left     (Mirror a) = liftM Mirror (right a)
   right    (Mirror a) = liftM Mirror (left a)
   downs    (Mirror a) = liftM Mirror (reverse (downs a))
   location (Mirror a) = location a

newtype PreOrder a = PreOrder a

instance Navigator a => Iterator (PreOrder a) where
   previous = liftWrapper (liftM rightMostLeaf . left >|< up)
   next = liftWrapper (down >|< rec)
    where rec = right >|< (up >=> rec)

newtype Comp f g a = Comp { unComp :: f (g a) }

instance Iterator (f (g a)) => Iterator (Comp f g a) where
   previous (Comp a) = liftM Comp (previous a)
   next     (Comp a) = liftM Comp (next a)

class Wrapper f where
   wrap   :: a -> f a
   unwrap :: f a -> a

liftWrapper :: Wrapper f => (a -> Maybe a) -> f a -> Maybe (f a)
liftWrapper f = fmap wrap . f . unwrap

unliftWrapper :: Wrapper f => (f a -> Maybe (f a)) -> a -> Maybe a
unliftWrapper f = fmap unwrap . f . wrap

mapWrapper :: Wrapper f => (a -> a) -> f a -> f a
mapWrapper f = wrap . f . unwrap

instance (Wrapper f, Wrapper g) => Wrapper (Comp f g) where
   wrap   = Comp . wrap . wrap
   unwrap = unwrap . unwrap . unComp

instance Wrapper PreOrder where
   wrap = PreOrder
   unwrap (PreOrder a) = a

instance Wrapper Mirror where
   wrap = Mirror
   unwrap (Mirror a) = a 

type PostOrder = Comp Mirror (Comp PreOrder Mirror)

makePostOrder :: a -> PostOrder a
makePostOrder = wrap

newtype BreadthFirstOrder a = BreadthFirstOrder a

instance Wrapper BreadthFirstOrder where
   wrap = BreadthFirstOrder
   unwrap (BreadthFirstOrder a) = a

depth :: Navigator a => a -> Int
depth a | null xs   = 0
        | otherwise = maximum (map depth xs) + 1
 where 
   xs = downs a

leftMostAt :: Navigator a => Int -> a -> Maybe a
leftMostAt n
   | n == 0    = Just
   | otherwise = (down >=> leftMostAt (n-1)) >|< (right >=> leftMostAt n)

rightMostAt :: Navigator a => Int -> a -> Maybe a
rightMostAt = mirrored . leftMostAt

instance Navigator a => Iterator (BreadthFirstOrder a) where
   first (BreadthFirstOrder a) = BreadthFirstOrder (top a)
   final (BreadthFirstOrder a) = BreadthFirstOrder (safe (rightMostAt (depth a)) a)

   previous (BreadthFirstOrder a) = liftM BreadthFirstOrder $
      (left >|< h 1 >|< f 0) a
    where
     h n = up >=> (k n >|< h (n+1))
     k n = left >=> (rightMostAt n  >|< k n)
     f n = (up >=> f (n+1)) >|< rightMostAt (n-1)
     
   next (BreadthFirstOrder a) = liftM BreadthFirstOrder $ 
      (right >|< h 1 >|< f 0) a
    where
      h n = up >=> (k n >|< h (n+1))
      k n = right >=> (leftMostAt n >|< k n)
      f n = (up >=> f (n+1)) >|< leftMostAt (n+1)

data T = T Int [T] deriving Show

instance Uniplate T where
   uniplate (T a xs) = plate (T a) ||* xs

root (T a _) = a
rootf f (T a xs) = liftM (\b -> T b xs) (f a)

ex = T 0 [T 1 [], T 2 [T 3 [], T 4 [T 5 []], T 6 []], T 7 [T 8 [T 9 []]]]

pre  = map (root . current . unwrap) (fixpl next (PreOrder (uniplateNavigator ex)))
post = map (root . current . unwrap) (fixpl next (makePostOrder (leftMostLeaf $ uniplateNavigator ex)))
po = map (root . current . unwrap) (fixpl next (makePostOrder (leftMostLeaf $ uniplateNavigator ex)))
post2 = map (root . current . unwrap) (fixpl previous (makePostOrder (uniplateNavigator ex)))
bf  = map (root . current . unwrap) (fixpl next (BreadthFirstOrder (uniplateNavigator ex)))
bf2  = map (root . current . unwrap) (fixpl previous (final $ BreadthFirstOrder (uniplateNavigator ex)))

test = bf == reverse bf2

uniplateNavigator :: Uniplate a => a -> UniplateNavigator a
uniplateNavigator = focus

--------------------------------------

newtype Horizontal a = Horizontal a

instance Wrapper Horizontal where
   wrap = Horizontal
   unwrap (Horizontal a) = a
   
instance Navigator a => Iterator (Horizontal a) where
   previous = liftWrapper left
   next     = liftWrapper right
   first    = mapWrapper  leftMost
   final    = mapWrapper  rightMost

unliftHorizontal :: (Horizontal a -> Maybe (Horizontal a)) -> a -> Maybe a
unliftHorizontal = unliftWrapper

changeM :: Update f => (a -> Maybe a) -> f a -> Maybe (f a)
changeM f a = liftM (`replace` a) (f (current a))

desc :: Navigator a => (a -> Maybe a) -> a -> Maybe a
desc f = down >=> f >=> up

leaf :: Navigator a => a -> Maybe a
leaf a = if isLeaf a then Just a else Nothing

-- visit all elements
visitAll :: Iterator a => (a -> Maybe a) -> a -> Maybe a
visitAll f = fmap (safe (next >=> visitAll f)) . f
      
-- visit exactly one element
visitOne :: Iterator a => (a -> Maybe a) -> a -> Maybe a
visitOne f = f >|< (next >=> visitOne f)

-- visit at least one element
visitSome :: Iterator a => (a -> Maybe a) -> a -> Maybe a
visitSome f = (fmap (visitMany f) . f) >|< (next >=> visitSome f)

-- visit the possible elements 
visitMany :: Iterator a => (a -> Maybe a) -> a -> a
visitMany f = safe (fmap (visitMany f) . next) . safe f

--
downAll :: Navigator a => (a -> Maybe a) -> a -> Maybe a
downAll = desc . unliftHorizontal . visitAll . liftWrapper

downOne :: Navigator a => (a -> Maybe a) -> a -> Maybe a
downOne = desc . unliftHorizontal . visitOne . liftWrapper

fulltd :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> Maybe (f a)
fulltd f = changeM f >=> (downAll (fulltd f) >|< leaf)

spinetd :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> Maybe (f a)
spinetd f = changeM f >=> (downOne (spinetd f) >|< leaf)

stoptd :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> Maybe (f a)
stoptd f = changeM f >|< downAll (stoptd f)

oncetd :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> Maybe (f a)
oncetd f = changeM f >|< downOne (oncetd f)

oncebu :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> Maybe (f a)
oncebu f = downOne (oncebu f) >|< changeM f

innermost :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> f a
innermost = fixp . oncebu

outermost :: (Update f, Navigator (f a)) => (a -> Maybe a) -> f a -> f a
outermost = fixp . oncetd

go = outermost (rootf exf) (uniplateNavigator ex)

exf n 
   | n >= 10   = Nothing
   | otherwise = Just (n+2)