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
module Common.Traversal.Navigator
   ( -- * Navigator type class
     Navigator(..), Location
   , isTop, isLeaf
   , hasLeft, hasRight, hasUp, hasDown
   , top, leftMost, rightMost, leftMostLeaf, rightMostLeaf
   , downTo, navigateTo, navigateTowards
     -- * Tree walks
   , PreOrder, makePreOrder
   , Leafs, makeLeafs
     -- * Uniplate navigator
   , UniplateNavigator
   ) where

import Common.Traversal.Utils
import Common.Traversal.Iterator
import Common.Utils.Uniplate
import Control.Monad
import Data.Generics.Str
import Data.Maybe

---------------------------------------------------------------
-- Navigator type class

type Location = [Int]

-- | For a minimal complete definition, provide an implemention for downs or
-- allDowns. All other functions need an implementation as well, except for
-- change. Note that a constructor (a -> f a) is not included in the type class
-- to allow additional type class constraints on type a.
class Navigator a where
   -- navigation
   up       :: a -> Maybe a
   down     :: a -> Maybe a
   downLast :: a -> Maybe a
   left     :: a -> Maybe a
   right    :: a -> Maybe a
   -- extra navigation
   downs    :: a -> [a]
   -- inspection
   location :: a -> Location
   arity    :: a -> Int
   -- default definitions
   downLast = liftM (fixp right) . down
   downs    = maybe [] (fixpl right) . down
   arity    = length . downs

isTop, isLeaf :: Navigator a => a -> Bool
isTop  = not . hasUp
isLeaf = not . hasDown

hasLeft, hasRight, hasUp, hasDown :: Navigator a => a -> Bool
hasLeft  = isJust . left
hasRight = isJust . right
hasUp    = isJust . up
hasDown  = isJust . down

top, leftMost, rightMost :: Navigator a => a -> a
top       = fixp up
leftMost  = fixp left
rightMost = fixp right

leftMostLeaf, rightMostLeaf :: Navigator a => a -> a
leftMostLeaf  = fixp down
rightMostLeaf = fixp downLast

downTo :: Navigator a => Int -> a -> Maybe a
downTo n
   | n < 0 = const Nothing
   | True  = listToMaybe . drop n . downs
   
navigateTo :: Navigator a => Location -> a -> Maybe a
navigateTo is a = go (navigation (location a) is) a 
 where 
   go = foldr (>=>) Just 

navigateTowards :: Navigator a => Location -> a -> a
navigateTowards is a = go (navigation (location a) is) a 
 where 
   go = foldr (\f g -> safe (fmap g . f)) id

navigation :: Navigator a => Location -> Location -> [a -> Maybe a]
navigation old new = replicate upnr up ++ map downTo ds
 where
   same = length (takeWhile id (zipWith (==) old new))
   upnr = length old - same
   ds   = drop same new

----------------------------------------------------------------
-- Tree walks

newtype PreOrder a = Pre { fromPre :: a }

makePreOrder :: a -> PreOrder a
makePreOrder = wrap

instance Wrapper PreOrder where
   wrap   = Pre
   unwrap = fromPre

instance Update PreOrder where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (PreOrder a) where
   previous = liftWrapper (liftM rightMostLeaf . left >|< up)
   next     = let rec = right >|< (up >=> rec)
              in liftWrapper (down >|< rec)
   first    = mapWrapper top
   final    = mapWrapper (rightMostLeaf . top)

newtype Leafs a = Leafs { fromLeafs :: a } 

makeLeafs :: a -> Leafs a
makeLeafs = wrap

instance Wrapper Leafs where
   wrap   = Leafs
   unwrap = fromLeafs
   
instance Update Leafs where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (Leafs a) where
   previous = liftWrapper $ 
      let rec = left >|< (up >=> rec)
      in liftM rightMostLeaf . rec
   next = liftWrapper $ 
      let rec = right >|< (up >=> rec)
      in liftM leftMostLeaf . rec
   first = mapWrapper (leftMostLeaf . top)
   final = mapWrapper (rightMostLeaf . top)

---------------------------------------------------------------
-- Str navigator (private)

data StrNavigator a = SN 
   { currentStr :: Str a
   , strContext :: [Either (Str a) (Str a)] 
   }

instance Navigator (StrNavigator a) where
   up (SN a (x:xs)) = Just (SN (either (flip Two) Two x a) xs)
   up _ = Nothing
   down (SN (Two a b) xs) = Just (SN a (Left b:xs))
   down _ = Nothing
   downLast (SN (Two a b) xs) = Just (SN b (Right a:xs))
   downLast _ = Nothing
   left (SN a (Right b:xs)) = Just (SN b (Left a:xs))
   left _ = Nothing
   right (SN a (Left b:xs)) = Just (SN b (Right a:xs))
   right _ = Nothing
   location = reverse . map (either (const 0) (const 1)) . strContext

instance Focus (StrNavigator a) where
   type Unfocus (StrNavigator a) = Str a
   focus   = flip SN []
   unfocus = currentStr . top

sizeStrNavigator :: StrNavigator a -> Int
sizeStrNavigator (SN a xs) = 
   sum (countStr a : map (either countStr countStr) xs)

countStr :: Str a -> Int
countStr Zero      = 0
countStr (One _)   = 1
countStr (Two a b) = countStr a + countStr b

---------------------------------------------------------------
-- Str iterator (private)

data StrIterator a = SI 
   { posSI  :: !Int
   , fromSI :: Leafs (StrNavigator a)
   }

instance Iterator (StrIterator a) where
   next     (SI n a) = liftM (SI (n+1)) $ searchNext ok a
   previous (SI n a) = liftM (SI (n-1)) $ searchPrevious ok a
   first    (SI _ a) = SI 0 $ safe (searchForward ok) (first a)
   final    (SI _ a) = finalSI $ safe (searchBackward ok) (final a)
   position          = posSI

instance Focus (StrIterator a) where
   type Unfocus (StrIterator a) = Str a
   focusM  = firstStrIterator
   unfocus = unfocus . unwrap . fromSI

instance Update StrIterator where
   update (SI n (Leafs a)) =
      case currentStr a of
         One b -> (b, \c -> SI n $ wrap $ a {currentStr = One c})
         _     -> error "unsafe update"

firstStrIterator :: Str a -> Maybe (StrIterator a)
firstStrIterator = fmap (SI 0) . searchForward ok . first . wrap . focus

lastStrIterator :: Str a -> Maybe (StrIterator a)
lastStrIterator = fmap finalSI . searchBackward ok . final . wrap . focus

finalSI :: Leafs (StrNavigator a) -> StrIterator a
finalSI a = SI (sizeStrNavigator (unwrap a) - 1) a

ok :: Wrapper f => f (StrNavigator a) -> Bool
ok = isOne . currentStr . unwrap
 where
   isOne (One _) = True
   isOne _       = False

---------------------------------------------------------------
-- Uniplate navigator

data UniplateNavigator a = U [StrIterator a -> StrIterator a] (StrIterator a)

instance (Show a, Uniplate a ) => Show (UniplateNavigator a) where
   show a = show (current a) ++ " @ " ++ show (location a)

instance Uniplate a => Navigator (UniplateNavigator a) where
   up (U [] _)     = Nothing
   up (U (f:fs) a) = Just (U fs (f a))
  
   down     = downWith focusM
   downLast = downWith lastStrIterator
      
   left  (U fs a) = liftM (U fs) (previous a)
   right (U fs a) = liftM (U fs) (next a)
      
   location (U fs a) = reverse (rec a fs)
    where
      rec _ [] = []
      rec b (g:gs) = position b : rec (g b) gs

instance Update UniplateNavigator where
   update (U xs a) = (current a, U xs . flip replace a)

instance Uniplate a => Focus (UniplateNavigator a) where
   type Unfocus (UniplateNavigator a) = a
   focus   = U [] . focus . One
   unfocus = current . top

downWith :: Uniplate a => (Str a -> Maybe (StrIterator a)) 
                       -> UniplateNavigator a -> Maybe (UniplateNavigator a)
downWith make (U fs a) = liftM (U (f:fs)) (make cs)
 where
   (cs, g) = uniplate (current a)
   f = (`replace` a) . g . unfocus