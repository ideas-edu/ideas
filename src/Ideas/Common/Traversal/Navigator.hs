{-# LANGUAGE TypeFamilies, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.Traversal.Navigator
   ( -- * Location information
     Location, toLocation, fromLocation
     -- * Navigator type class
   , Navigator(..)
   , isTop, isLeaf
   , hasLeft, hasRight, hasUp, hasDown
   , top, leftMost, rightMost, leftMostLeaf, rightMostLeaf
   , depth, level, levelNext, levelPrevious, leftMostAt, rightMostAt
   , downs, downTo, arity, navigateTo, navigateTowards
     -- * Tree walks
   , PreOrder, makePreOrder
   , PostOrder, makePostOrder
   , LevelOrder, makeLevelOrder
   , Horizontal, makeHorizontal
   , Leafs, makeLeafs
     -- * Uniplate navigator
   , UniplateNavigator
   ) where

import Control.Monad
import Data.Function
import Data.Generics.Str
import Data.Maybe
import Data.Semigroup as Sem
import Ideas.Common.Traversal.Iterator
import Ideas.Common.Traversal.Utils
import Ideas.Utils.Uniplate
import Test.QuickCheck

---------------------------------------------------------------
-- Navigator type class

newtype Location = L { fromLocation :: [Int] }
 deriving (Eq, Ord)

instance Show Location where
   show = show . fromLocation

instance Sem.Semigroup Location where
   L xs <> L ys = L (xs ++ ys)

instance Monoid Location where
   mempty  = L []
   mappend = (<>)

toLocation :: [Int] -> Location
toLocation = L

-- | For a minimal complete definition, provide an implemention for downs or
-- allDowns. All other functions need an implementation as well, except for
-- change. Note that a constructor (a -> f a) is not included in the type class
-- to allow additional type class constraints on type a.
class Navigator a where
   up       :: a -> Maybe a
   down     :: a -> Maybe a
   downLast :: a -> Maybe a
   left     :: a -> Maybe a
   right    :: a -> Maybe a
   childnr  :: a -> Int
   location :: a -> Location
   -- default definitions
   downLast = fmap (fixp right) . down
   childnr  = pred . length . fixpl left
   location = toLocation . map childnr . drop 1 . reverse . fixpl up

instance Navigator a => Navigator (Mirror a) where
   up       = liftWrapper up
   down     = liftWrapper downLast
   downLast = liftWrapper down
   left     = liftWrapper right
   right    = liftWrapper left

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

downs :: Navigator a => a -> [a]
downs = maybe [] (fixpl right) . down

downTo :: Navigator a => Int -> a -> Maybe a
downTo n
   | n < 0     = const Nothing
   | otherwise = listToMaybe . drop n . downs

arity :: Navigator a => a -> Int
arity = length . downs

depth :: Navigator a => a -> Int
depth a | null xs   = 0
        | otherwise = maximum (map depth xs) + 1
 where
   xs = downs a

level :: Navigator a => a -> Int
level = pred . length . fixpl up

levelNext :: Navigator a => a -> Maybe a
levelNext = right >|< f 1
 where
   f n = up >=> (g n >|< f (n+1))
   g n = right >=> (leftMostAt n >|< g n)

levelPrevious :: Navigator a => a -> Maybe a
levelPrevious = fmap unwrap . levelNext . makeMirror

leftMostAt :: Navigator a => Int -> a -> Maybe a
leftMostAt n
   | n == 0    = Just
   | n <  0    = const Nothing
   | otherwise = (down >=> leftMostAt (n-1)) >|< (right >=> leftMostAt n)

rightMostAt :: Navigator a => Int -> a -> Maybe a
rightMostAt n = fmap unwrap . leftMostAt n . makeMirror

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
   os   = fromLocation old
   ns   = fromLocation new
   same = length (takeWhile id (zipWith (==) os ns))
   upnr = length os - same
   ds   = drop same ns

----------------------------------------------------------------
-- Tree walks

newtype PreOrder a = Pre { fromPre :: a }
   deriving (Show, Eq)

makePreOrder :: a -> PreOrder a
makePreOrder = wrap

instance Wrapper PreOrder where
   wrap   = Pre
   unwrap = fromPre

instance Update PreOrder where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (PreOrder a) where
   previous = liftWrapper ((fmap rightMostLeaf . left) >|< up)
   next     = let rec = right >|< (up >=> rec)
              in liftWrapper (down >|< rec)
   first    = mapWrapper top
   final    = mapWrapper (rightMostLeaf . top)

newtype PostOrder a = Post { fromPost :: Mirror (PreOrder (Mirror a))}
   deriving (Show, Eq, Iterator)

instance Wrapper PostOrder where
   wrap   = Post . wrap . wrap . wrap
   unwrap = unwrap . unwrap . unwrap . fromPost

instance Update PostOrder where
   update a = (unwrap a, wrap)

makePostOrder :: a -> PostOrder a
makePostOrder = wrap

newtype LevelOrder a = Level { fromLevel :: a } -- breadth-first
   deriving (Show, Eq)

instance Wrapper LevelOrder where
   wrap   = Level
   unwrap = fromLevel

instance Update LevelOrder where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (LevelOrder a) where
   previous = let f a = rightMostAt (level a-1) (top a)
              in liftWrapper (levelPrevious >|< f)
   next     = let f a = leftMostAt (level a+1) (top a)
              in liftWrapper (levelNext >|< f)
   first    = mapWrapper top
   final    = mapWrapper $ \a -> safe (rightMostAt (depth (top a))) (top a)

makeLevelOrder :: a -> LevelOrder a
makeLevelOrder = wrap

newtype Horizontal a = Hor { fromHor :: a }
   deriving (Show, Eq)

instance Wrapper Horizontal where
   wrap   = Hor
   unwrap = fromHor

instance Update Horizontal where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (Horizontal a) where
   previous = liftWrapper left
   next     = liftWrapper right
   first    = mapWrapper leftMost
   final    = mapWrapper rightMost
   position = childnr . unwrap

makeHorizontal :: a -> Horizontal a
makeHorizontal = wrap

newtype Leafs a = Leafs { fromLeafs :: a }
   deriving (Show, Eq)

makeLeafs :: Navigator a => a -> Leafs a
makeLeafs = first . wrap

instance Wrapper Leafs where
   wrap   = Leafs
   unwrap = fromLeafs

instance Update Leafs where
   update a = (unwrap a, wrap)

instance Navigator a => Iterator (Leafs a) where
   previous = liftWrapper $
      let rec = left >|< (up >=> rec)
      in fmap rightMostLeaf . rec
   next = liftWrapper $
      let rec = right >|< (up >=> rec)
      in fmap leftMostLeaf . rec
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
   childnr  = maybe 0 (either (const 0) (const 1)) . listToMaybe . strContext

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
   next     (SI n a) = SI (n+1) <$> searchNext ok a
   previous (SI n a) = SI (n-1) <$> searchPrevious ok a
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

instance (Show a, Uniplate a) => Show (UniplateNavigator a) where
   show a = show (current a) ++ " @ " ++ show (location a)

instance (Eq a, Uniplate a) => Eq (UniplateNavigator a) where
   (==) = on (==) $ \a -> (current a, unfocus a, location a)

instance Uniplate a => Navigator (UniplateNavigator a) where
   up (U [] _)     = Nothing
   up (U (f:fs) a) = Just (U fs (f a))

   down     = downWith focusM
   downLast = downWith lastStrIterator

   left  (U fs a) = U fs <$> previous a
   right (U fs a) = U fs <$> next a

   childnr (U _ a) = position a

instance Update UniplateNavigator where
   update (U xs a) = (current a, U xs . flip replace a)

instance Uniplate a => Focus (UniplateNavigator a) where
   type Unfocus (UniplateNavigator a) = a
   focus   = U [] . focus . One
   unfocus = current . top

instance (Arbitrary a, Uniplate a) => Arbitrary (UniplateNavigator a) where
   arbitrary = fmap focus arbitrary >>= genNav
    where
      genNav a =
         case map genNav (downs a) of
            [] -> return a
            xs -> frequency [(1, return a), (4, oneof xs)]

downWith :: Uniplate a => (Str a -> Maybe (StrIterator a))
                       -> UniplateNavigator a -> Maybe (UniplateNavigator a)
downWith make (U fs a) = U (f:fs) <$> make cs
 where
   (cs, g) = uniplate (current a)
   f = (`replace` a) . g . unfocus