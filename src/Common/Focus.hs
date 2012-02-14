{-# LANGUAGE TypeFamilies #-}
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
module Common.Focus
   ( Update(..), Focus(..)
   , current, change, replace, leave
   , UniplateNavigator, uniplateNavigator
   , ListIterator, StrIterator
   ) where

import Common.Navigator
import Common.Utils.Uniplate
import Control.Monad
import Data.List
import Data.Maybe
import Data.Generics.Str

class Update f where
   update  :: f a -> (a, a -> f a)

current :: Update f => f a -> a
current  = fst . update 

change  :: Update f => (a -> a) -> f a -> f a
change f = (\(x, g) -> g (f x)) . update

replace :: Update f => a -> f a -> f a
replace  = change . const

leave :: (Update f, Navigator (f a)) => f a -> a
leave = current . top

---------------------------------------------------------------

data ListIterator a = LI [a] a [a]
   
instance Show a => Show (ListIterator a) where
   show (LI xs y ys) = 
      let listLike   = brackets . intercalate ","
          brackets s = "[" ++ s ++ "]"
          focusOn  s = "<<" ++ s ++ ">>"
      in listLike (map show (reverse xs) ++ focusOn (show y) : map show ys)
   
instance Update ListIterator where
   update (LI xs a ys) = (a, \b -> LI xs b ys)
   
instance Iterator (ListIterator a) where
   previous (LI (x:xs) y ys) = Just (LI xs x (y:ys))
   previous _                = Nothing
   
   next (LI xs x (y:ys)) = Just (LI (x:xs) y ys)
   next _                = Nothing

fromListIterator :: ListIterator a -> [a]
fromListIterator (LI xs y ys) = reverse xs ++ y : ys

toListIterator :: [a] -> Maybe (ListIterator a)
toListIterator []     = Nothing
toListIterator (x:xs) = Just (LI [] x xs)

--posListIterator :: ListIterator a -> Int
--posListIterator (LI xs _ _) = length xs

---------------------------------------------------------------

data StrIterator a = SI a !Int [Either (Str a) (Str a)]

instance Show a => Show (StrIterator a) where
   show a = show (current a) ++ " @ " ++ show (posStrIterator a)

instance Update StrIterator where
   update (SI a n xs) = (a, \b -> SI b n xs)
   
instance Iterator (StrIterator a) where
   previous (SI a n xs) = rec xs (One a)
    where
      rec []     _ = Nothing
      rec (y:ys) s = 
         let make b = case lastStrIterator b of
                         Just (SI c _ zs) -> Just (SI c (n-1) (zs++Left s:ys))
                         Nothing -> rec ys (b `Two` s)
         in either (rec ys . (s `Two`)) make y 

   next (SI a n xs) = rec xs (One a)
    where
      rec [] _ = Nothing
      rec (y:ys) s = 
         let make b = case firstStrIterator b of
                         Just (SI c _ zs) -> Just (SI c (n+1) (zs++Right s:ys))
                         Nothing -> rec ys (s `Two` b)
         in either make (rec ys . (`Two` s)) y

fromStrIterator :: StrIterator a -> Str a
fromStrIterator (SI a _ xs) = rec xs (One a)
 where
   rec []     s = s
   rec (y:ys) s = rec ys (either (s `Two`) (`Two` s) y)

singleStrIterator :: a -> StrIterator a
singleStrIterator a = SI a 0 []

firstStrIterator :: Str a -> Maybe (StrIterator a)
firstStrIterator = rec []
 where
   rec acc str =
      case str of
         Zero    -> Nothing
         One a   -> Just (SI a 0 acc)
         Two a b -> rec (Left b:acc) a  
                `mplus`
                    rec (Right a:acc) b

lastStrIterator :: Str a -> Maybe (StrIterator a)
lastStrIterator = rec 0 []
 where
   rec n acc str = 
      case str of
         Zero    -> Nothing
         One a   -> Just (SI a n acc)
         Two a b -> 
            rec (n+countStr a) (Right a:acc) b 
          `mplus` 
            rec n (Left b:acc) a

posStrIterator :: StrIterator a -> Int
posStrIterator (SI _ n _) = n

countStr :: Str a -> Int
countStr Zero      = 0
countStr (One _)   = 1
countStr (Two a b) = countStr a + countStr b

-- s = firstStrIterator $ Two (Two (One 1) (One 2)) (Two (Two (One 3) (One 4)) (One 5))

---------------------------------------------------------------

data UniplateNavigator a = U [StrIterator a -> StrIterator a] (StrIterator a)

instance (Show a, Uniplate a ) => Show (UniplateNavigator a) where
   show a = show (current a) ++ " @ " ++ show (location a)

instance Uniplate a => Navigator (UniplateNavigator a) where
   up (U [] _)     = Nothing
   up (U (f:fs) a) = Just (U fs (f a))
  
   down     = downWith firstStrIterator
   downLast = downWith lastStrIterator
      
   left  (U fs a) = liftM (U fs) (previous a)
   right (U fs a) = liftM (U fs) (next a)
      
   location (U fs a) = reverse (rec a fs)
    where
      rec _ [] = []
      rec b (g:gs) = posStrIterator b : rec (g b) gs

instance Update UniplateNavigator where
   update (U xs a) = (current a, U xs . flip replace a)

downWith :: Uniplate a => (Str a -> Maybe (StrIterator a)) 
                       -> UniplateNavigator a -> Maybe (UniplateNavigator a)
downWith make (U fs a) = liftM (U (f:fs)) (make cs)
 where
   (cs, g) = uniplate (current a)
   f x = replace (g (fromStrIterator x)) a

uniplateNavigator :: a -> UniplateNavigator a
uniplateNavigator a = U [] (singleStrIterator a)
   
{-
data T = T Int [T] deriving Show

instance Uniplate T where
   uniplate (T a xs) = plate (T a) ||* xs
-} 
--ex = U (singleStrIterator $
   --T 0 [T 1 [], T 2 [T 3 [], T 4 [], T 5 []], T 6 []]) []
   
-----------------------

class Focus a where
   type Unfocus a
   focus   :: Unfocus a -> a
   unfocus :: a -> Unfocus a
   
instance Focus (ListIterator a) where
   type Unfocus (ListIterator a) = [a]
   focus = fromJust . toListIterator
   unfocus = fromListIterator
 
instance Focus (StrIterator a) where
   type Unfocus (StrIterator a) = Str a
   focus   = fromJust . firstStrIterator
   unfocus = fromStrIterator

instance Uniplate a => Focus (UniplateNavigator a) where
   type Unfocus (UniplateNavigator a) = a
   focus   = uniplateNavigator
   unfocus = leave