{-# OPTIONS -XGeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A hierarchy of magma's (binary operators) and groups, up to Abelian groups.
--
-----------------------------------------------------------------------------
module Common.Rewriting.Group 
   ( -- Magma
     IsMagma(..), Magma, magma, magmaView, magmaListView
   , withMatch, findMagma
   , isAssociative, isCommutative, isIdempotent
   , makeAssociative, makeCommutative, makeIdempotent
     -- Semigroup
   , IsSemiGroup(..), SemiGroup, semiGroup
   , leftIsPreferred, rightIsPreferred
   , preferLeft, preferRight
     -- Monoid
   , IsMonoid(..), Monoid, monoid
     -- Group
   , IsGroup(..), Group, group
     -- Abelian group
   , IsAbelianGroup(..), AbelianGroup, abelianGroup
   ) where

import Control.Arrow
import Common.Id
import Common.View hiding (identity)

-------------------------------------------------------------------
-- * Magma

class IsMagma f where 
   operation   :: f a -> a -> a -> a
   hasMagma    :: f a -> (Magma a, Magma a -> f a)
   toMagma     :: f a -> Magma a
   changeMagma :: (Magma a -> Magma a) -> f a -> f a
   -- default definitions
   operation     = magmaOperation . toMagma
   toMagma       = fst . hasMagma
   changeMagma f = uncurry (flip ($)) .  first f . hasMagma

data Magma a = Magma
   { magmaId         :: Id
   , magmaOperation  :: a -> a -> a
   , magmaMatch      :: a -> Maybe (a, a)
   , magmaProperties :: [MagmaProperty]
   }

data MagmaProperty = Associative | Commutative | Idempotent | PreferLeft
   deriving Eq

instance Show (Magma a) where
   show m = "Magma " ++ showId m

instance HasId (Magma a) where
   getId = magmaId
   changeId f m = m {magmaId = f (magmaId m)}

instance IsMagma Magma where 
   hasMagma a = (a, id)

magma :: IsId n => n -> (a -> a -> a) -> Magma a
magma n op = Magma (newId n) op (const Nothing) []

magmaView :: IsMagma m => m a -> View a (a, a)
magmaView = f . toMagma
 where
   f m = makeView (magmaMatch m) (uncurry $ operation m)

-- The list can (and should) only contain more than two elements if the magma 
-- is associative
magmaListView :: IsMagma m => m a -> View a [a]
magmaListView m = makeView (Just . toList) fromList
 where
   toList = if isAssociative m then ($ []) . rec else f
 
   f a = maybe [a] (\(x, y) -> [x, y]) (match (magmaView m) a)
 
   rec a = case match (magmaView m) a of
            Just (b, c) -> rec b . rec c
            Nothing     -> (a:)

   fromList xs
      | null xs =
           error "semiGroupView.build: empty list"
      | n>2 && not (isAssociative m) =
           error $ "semiGroupView.build: not associativity for " 
                   ++ showId (toMagma m)
      | otherwise = fold (operation m) xs
    where
      n    = length xs
      fold = if hasProperty PreferLeft m then foldl1 else foldr1

withMatch :: IsMagma m => (a -> Maybe (a, a)) -> m a -> m a
withMatch f = changeMagma $ \m -> m {magmaMatch = f}

isAssociative, isCommutative, isIdempotent :: IsMagma m => m a -> Bool
isAssociative = hasProperty Associative
isCommutative = hasProperty Commutative
isIdempotent  = hasProperty Idempotent

makeAssociative, makeCommutative, makeIdempotent :: IsMagma m => m a -> m a
makeAssociative = giveProperty Associative
makeCommutative = giveProperty Commutative
makeIdempotent  = giveProperty Idempotent

findMagma :: IsMagma m => (m a -> b) -> m a -> (Magma a, Magma a -> b)
findMagma f = second (f .) . hasMagma

-- helper functions
hasProperty :: IsMagma m => MagmaProperty -> m a -> Bool
hasProperty p = elem p . magmaProperties . toMagma

giveProperty :: IsMagma m => MagmaProperty -> m a -> m a
giveProperty p = changeMagma $ \m -> 
   m {magmaProperties = p:magmaProperties m}

removeProperty :: IsMagma m => MagmaProperty -> m a -> m a
removeProperty p = changeMagma $ \m -> 
   m {magmaProperties = filter (/=p) (magmaProperties m)}

-------------------------------------------------------------------
-- * SemiGroup

class IsMagma f => IsSemiGroup f where
   toSemiGroup :: f a -> SemiGroup a
   -- default definition
   toSemiGroup = SemiGroup . toMagma

newtype SemiGroup a = SemiGroup (Magma a)
   deriving (HasId, IsMagma)

instance Show (SemiGroup a) where
   show m = "Semigroup " ++ showId m

instance IsSemiGroup SemiGroup where
   toSemiGroup = id

semiGroup :: IsId n => n -> (a -> a -> a) -> SemiGroup a
semiGroup n op = makeAssociative $ SemiGroup (magma n op)

leftIsPreferred, rightIsPreferred :: IsSemiGroup m => m a -> Bool
leftIsPreferred  = hasProperty PreferLeft
rightIsPreferred = not . leftIsPreferred

preferLeft, preferRight :: IsSemiGroup m => m a -> m a
preferLeft  = giveProperty PreferLeft
preferRight = removeProperty PreferLeft

-------------------------------------------------------------------
-- * Monoid

class IsSemiGroup f => IsMonoid f where
   identity :: f a -> a
   toMonoid :: f a -> Monoid a
   -- default definition
   toMonoid m = Monoid (identity m) (toSemiGroup m)

data Monoid a = Monoid a (SemiGroup a)

instance Show (Monoid a) where
   show m = "Monoid " ++ showId m

instance HasId (Monoid a) where
   getId    = getId . toMagma
   changeId = changeMagma . changeId

instance IsMagma Monoid where
   hasMagma (Monoid e g) = findMagma (Monoid e) g

instance IsSemiGroup Monoid

instance IsMonoid Monoid where
   identity (Monoid e _) = e
   toMonoid = id

monoid :: IsId n => n -> (a -> a -> a) -> a -> Monoid a
monoid n op e = Monoid e (semiGroup n op)

-------------------------------------------------------------------
-- * Group

class IsMonoid f => IsGroup f where
   inverse :: f a -> a -> a
   toGroup :: f a -> Group a
   -- default definition
   toGroup g = Group (inverse g) (toMonoid g)

data Group a = Group (a -> a) (Monoid a)

instance Show (Group a) where
   show m = "Group " ++ showId m

instance HasId (Group a) where
   getId    = getId . toMagma
   changeId = changeMagma . changeId

instance IsMagma Group where
   hasMagma (Group inv m) = findMagma (Group inv) m

instance IsSemiGroup Group

instance IsMonoid Group where
   identity (Group _ m) = identity m

instance IsGroup Group where
   inverse (Group inv _) = inv
   toGroup = id

group :: IsId n => n -> (a -> a -> a) -> a -> (a -> a) -> Group a
group n op e inv = Group inv (monoid n op e)

-------------------------------------------------------------------
-- * Abelian Group

class IsGroup f => IsAbelianGroup f where
   toAbelianGroup :: f a -> AbelianGroup a
   -- default definition
   toAbelianGroup = AbelianGroup . toGroup

newtype AbelianGroup a = AbelianGroup (Group a)
   deriving (HasId, IsMagma, IsSemiGroup, IsMonoid, IsGroup)

abelianGroup :: IsId n => n -> (a -> a -> a) -> a -> (a -> a) -> AbelianGroup a
abelianGroup n op e inv = makeCommutative $ AbelianGroup (group n op e inv)

instance Show (AbelianGroup a) where
   show m = "Abelian group " ++ showId m
   
instance IsAbelianGroup AbelianGroup