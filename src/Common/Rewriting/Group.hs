{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Common.Rewriting.Operator

-------------------------------------------------------------------
-- * Magma

class IsMagma f where 
   operation   :: f a -> a -> a -> a
   hasMagma    :: f a -> (Magma a, Magma a -> f a)
   toMagma     :: f a -> Magma a
   changeMagma :: (Magma a -> Magma a) -> f a -> f a
   -- default definitions
   operation     = binary . magmaBinaryOp . toMagma
   toMagma       = fst . hasMagma
   changeMagma f = uncurry (flip ($)) . first f . hasMagma

data Magma a = Magma
   { magmaBinaryOp   :: BinaryOp a
   , magmaProperties :: [MagmaProperty]
   }

data MagmaProperty = Associative | Commutative | Idempotent | PreferLeft
   deriving Eq

instance Show (Magma a) where
   show m = "Magma " ++ showId m

instance HasId (Magma a) where
   getId = getId . magmaBinaryOp
   changeId f m = m {magmaBinaryOp = changeId f (magmaBinaryOp m)}

instance IsMagma Magma where
   hasMagma a = (a, id)

magma :: BinaryOp a -> Magma a
magma op = Magma op []

magmaView :: IsMagma m => m a -> View a (a, a)
magmaView = binaryView . magmaBinaryOp . toMagma

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
withMatch f = changeMagma $ \m -> m {magmaBinaryOp = g (magmaBinaryOp m)}
 where
   g op = makeBinary (getId op) (binary op) f

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
   toSemiGroup m = SemiGroup (rightIsPreferred m) (toMagma m)

data SemiGroup a = SemiGroup Bool (Magma a)

instance Show (SemiGroup a) where
   show m = "Semigroup " ++ showId m

instance HasId (SemiGroup a) where
   getId    = getId . toMagma
   changeId = changeMagma . changeId

instance IsMagma SemiGroup where
   hasMagma (SemiGroup b m) = findMagma (SemiGroup b) m

instance IsSemiGroup SemiGroup where
   toSemiGroup = id

semiGroup :: BinaryOp a -> SemiGroup a
semiGroup op = makeAssociative $ SemiGroup True (magma op)

leftIsPreferred, rightIsPreferred :: IsSemiGroup m => m a -> Bool
leftIsPreferred  = hasProperty PreferLeft
rightIsPreferred = not . leftIsPreferred

preferLeft, preferRight :: IsSemiGroup m => m a -> m a
preferLeft  = giveProperty PreferLeft
preferRight = removeProperty PreferLeft

-------------------------------------------------------------------
-- * Monoid

class IsSemiGroup f => IsMonoid f where
   identity    :: f a -> a
   identityCon :: f a -> Constant a
   toMonoid    :: f a -> Monoid a
   -- default definition
   identity   = constant . identityCon
   toMonoid m = Monoid (identityCon m) (toSemiGroup m)

data Monoid a = Monoid (Constant a) (SemiGroup a)

instance Show (Monoid a) where
   show m = "Monoid " ++ showId m

instance HasId (Monoid a) where
   getId    = getId . toMagma
   changeId = changeMagma . changeId

instance IsMagma Monoid where
   hasMagma (Monoid e g) = findMagma (Monoid e) g

instance IsSemiGroup Monoid

instance IsMonoid Monoid where
   identityCon (Monoid e _) = e
   toMonoid = id

monoid :: BinaryOp a -> Constant a -> Monoid a
monoid op e = Monoid e (semiGroup op)

-------------------------------------------------------------------
-- * Group

class IsMonoid f => IsGroup f where
   inverse   :: f a -> a -> a
   inverseOp :: f a -> UnaryOp a
   toGroup   :: f a -> Group a
   -- default definition
   inverse   = unary . inverseOp
   toGroup g = Group (inverseOp g) (toMonoid g)

data Group a = Group (UnaryOp a) (Monoid a)

instance Show (Group a) where
   show m = "Group " ++ showId m

instance HasId (Group a) where
   getId    = getId . toMagma
   changeId = changeMagma . changeId

instance IsMagma Group where
   hasMagma (Group inv m) = findMagma (Group inv) m

instance IsSemiGroup Group

instance IsMonoid Group where
   identityCon (Group _ m) = identityCon m

instance IsGroup Group where
   inverseOp (Group inv _) = inv
   toGroup = id

group :: BinaryOp a -> Constant a -> UnaryOp a -> Group a
group op e inv = Group inv (monoid op e)

-------------------------------------------------------------------
-- * Abelian Group

class IsGroup f => IsAbelianGroup f where
   toAbelianGroup :: f a -> AbelianGroup a
   -- default definition
   toAbelianGroup = AbelianGroup . toGroup

newtype AbelianGroup a = AbelianGroup (Group a)
   deriving (HasId, IsMagma, IsSemiGroup, IsMonoid, IsGroup)

abelianGroup :: BinaryOp a -> Constant a -> UnaryOp a -> AbelianGroup a
abelianGroup op e inv = makeCommutative $ AbelianGroup (group op e inv)

instance Show (AbelianGroup a) where
   show m = "Abelian group " ++ showId m
   
instance IsAbelianGroup AbelianGroup