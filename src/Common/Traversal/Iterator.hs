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
module Common.Traversal.Iterator
   ( -- * Iterator type class
     Iterator(..), isFirst, isFinal, hasNext, hasPrevious
     -- * List iterator
   , ListIterator
     -- * Str iterator
   , StrIterator, lastStrIterator
   ) where

import Common.Traversal.Utils
import Control.Monad
import Data.Generics.Str
import Data.List
import Data.Maybe

---------------------------------------------------------------
-- Iterator type class

class Iterator a where
   next     :: a -> Maybe a
   previous :: a -> Maybe a
   first    :: a -> a
   final    :: a -> a
   position :: a -> Int
   -- default implementations
   first = fixp previous
   final = fixp next

isFirst :: Iterator a => a -> Bool
isFirst = not . hasPrevious

isFinal :: Iterator a => a -> Bool
isFinal = not . hasNext

hasNext :: Iterator a => a -> Bool
hasNext = isJust . next

hasPrevious :: Iterator a => a -> Bool
hasPrevious = isJust . previous

---------------------------------------------------------------
-- List iterator

data ListIterator a = LI [a] a [a]
   
instance Show a => Show (ListIterator a) where
   show (LI xs y ys) = 
      let listLike   = brackets . intercalate ","
          brackets s = "[" ++ s ++ "]"
          focusOn  s = "<<" ++ s ++ ">>"
      in listLike (map show (reverse xs) ++ focusOn (show y) : map show ys)
      
instance Iterator (ListIterator a) where
   previous (LI (x:xs) y ys) = Just (LI xs x (y:ys))
   previous _                = Nothing
      
   next     (LI xs x (y:ys)) = Just (LI (x:xs) y ys)
   next     _                = Nothing
   
   position (LI xs _ _) = length xs

instance Focus (ListIterator a) where
   type Unfocus (ListIterator a) = [a]
   
   focusM []     = Nothing
   focusM (x:xs) = Just (LI [] x xs)
   
   unfocus (LI xs y ys) = reverse xs ++ y : ys

instance Update ListIterator where
   update (LI xs a ys) = (a, \b -> LI xs b ys)

---------------------------------------------------------------
-- Str iterator

data StrIterator a = SI a !Int [Either (Str a) (Str a)]

instance Show a => Show (StrIterator a) where
   show a = show (current a) ++ " @ " ++ show (position a)

instance Update StrIterator where
   update (SI a n xs) = (a, \b -> SI b n xs)

-- to do: add first and final
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

   position (SI _ n _) = n

instance Focus (StrIterator a) where
   type Unfocus (StrIterator a) = Str a
   focusM  = firstStrIterator
   unfocus = fromStrIterator

fromStrIterator :: StrIterator a -> Str a
fromStrIterator (SI a _ xs) = rec xs (One a)
 where
   rec []     s = s
   rec (y:ys) s = rec ys (either (s `Two`) (`Two` s) y)

firstStrIterator :: Str a -> Maybe (StrIterator a)
firstStrIterator = rec []
 where
   rec acc str =
      case str of
         Zero    -> Nothing
         One a   -> Just (SI a 0 acc)
         Two a b -> rec (Left b:acc) a `mplus` rec (Right a:acc) b

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

countStr :: Str a -> Int
countStr Zero      = 0
countStr (One _)   = 1
countStr (Two a b) = countStr a + countStr b