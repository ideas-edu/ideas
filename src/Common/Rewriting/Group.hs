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
     IsMagma(..), Magma, magma, magmaView
   , withMatch, findMagma
   , isAssociative, isCommutative, isIdempotent
   , makeAssociative, makeCommutative, makeIdempotent
     -- Semigroup
   , IsSemiGroup, SemiGroup, semiGroup
   , leftIsPreferred, rightIsPreferred
   , preferLeft, preferRight
   , leftAssociative, rightAssociative, associative
     -- Monoid
   , IsMonoid(..), Monoid, monoid
   , leftIdentity, rightIdentity
     -- Group
   , IsGroup(..), Group, group
   , leftInverse, rightInverse
   , inverseIdentity, inverseTwice
   , flippedInverseDistribution
     -- Abelian group
   , IsAbelianGroup, AbelianGroup, abelianGroup
   , commutative, inverseDistribution
   ) where

import Control.Arrow
import Common.Id
import Common.Rewriting
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
   f m = newView (getId m) (magmaMatch m) (uncurry $ operation m)

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

rule :: (IsMagma m, IsId n, RuleBuilder f a, Rewrite a) => m a -> n -> f -> RewriteRule a
rule m s = rewriteRule (getId (toMagma m), s)

-------------------------------------------------------------------
-- * SemiGroup

class IsMagma a => IsSemiGroup a

newtype SemiGroup a = SemiGroup (Magma a)
   deriving (HasId, IsMagma)

instance Show (SemiGroup a) where
   show m = "Semigroup " ++ showId m

instance IsSemiGroup SemiGroup

semiGroup :: IsId n => n -> (a -> a -> a) -> SemiGroup a
semiGroup n op = makeAssociative $ SemiGroup (magma n op)

leftIsPreferred, rightIsPreferred :: IsSemiGroup m => m a -> Bool
leftIsPreferred  = hasProperty PreferLeft
rightIsPreferred = not . leftIsPreferred

preferLeft, preferRight :: IsSemiGroup m => m a -> m a
preferLeft  = giveProperty PreferLeft
preferRight = removeProperty PreferLeft

leftAssociative :: (IsSemiGroup m, Different a, Rewrite a) => m a -> RewriteRule a
leftAssociative m = rule m "associative.left" $ 
   \x y z -> x.(y.z) :~> (x.y).z
 where 
   (.) = operation m

rightAssociative :: (IsSemiGroup m, Different a, Rewrite a) => m a -> RewriteRule a
rightAssociative m = rule m "associative.right" $ 
   \x y z -> (x.y).z :~> x.(y.z)
 where 
   (.) = operation m

associative :: (IsSemiGroup m, Different a, Rewrite a) => m a -> RewriteRule a
associative m
   | leftIsPreferred m = leftAssociative m
   | otherwise         = rightAssociative m

-------------------------------------------------------------------
-- * Monoid

class IsSemiGroup f => IsMonoid f where
   identity :: f a -> a

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

monoid :: IsId n => n -> (a -> a -> a) -> a -> Monoid a
monoid n op e = Monoid e (semiGroup n op)

leftIdentity :: (IsMonoid m, Different a, Rewrite a) => m a -> RewriteRule a
leftIdentity m = rule m "identity.left" $ 
   \x -> e.x :~> x
 where 
   (.) = operation m
   e   = identity m

rightIdentity :: (IsMonoid m, Different a, Rewrite a) => m a -> RewriteRule a
rightIdentity m = rule m "identity.right" $ 
   \x -> x.e :~> x
 where 
   (.) = operation m
   e   = identity m

-------------------------------------------------------------------
-- * Group

class IsMonoid f => IsGroup f where
   inverse :: f a -> a -> a

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

group :: IsId n => n -> (a -> a -> a) -> a -> (a -> a) -> Group a
group n op e inv = Group inv (monoid n op e)

leftInverse :: (IsGroup m, Different a, Rewrite a) => m a -> RewriteRule a
leftInverse m = rule m "inverse.left" $ 
   \x -> f x.x :~> e
 where 
   (.) = operation m
   e   = identity m
   f   = inverse m

rightInverse :: (IsGroup m, Different a, Rewrite a) => m a -> RewriteRule a
rightInverse m = rule m "inverse.right" $ 
   \x -> x.f x :~> e
 where 
   (.) = operation m
   e   = identity m
   f   = inverse m

inverseIdentity :: (IsGroup m, Different a, Rewrite a) => m a -> RewriteRule a
inverseIdentity m = rule m "inverse.identity" $ 
   f e :~> e
 where
   e = identity m
   f = inverse m

inverseTwice :: (IsGroup m, Different a, Rewrite a) => m a -> RewriteRule a
inverseTwice m = rule m "inverse.twice" $ 
   \x -> f (f x) :~> x
 where 
   f = inverse m

flippedInverseDistribution :: (IsGroup m, Different a, Rewrite a) => m a -> RewriteRule a
flippedInverseDistribution m = rule m "inverse.distribution.flipped" $ 
   \x y -> f (x.y) :~> f y.f x
 where 
   (.) = operation m
   f   = inverse m

-------------------------------------------------------------------
-- * Abelian Group

class IsGroup a => IsAbelianGroup a

newtype AbelianGroup a = AbelianGroup (Group a)
   deriving (HasId, IsMagma, IsSemiGroup, IsMonoid, IsGroup)

abelianGroup :: IsId n => n -> (a -> a -> a) -> a -> (a -> a) -> AbelianGroup a
abelianGroup n op e inv = makeCommutative $ AbelianGroup (group n op e inv)

instance Show (AbelianGroup a) where
   show m = "Abelian group " ++ showId m
   
instance IsAbelianGroup AbelianGroup

-- The type class constraint IsAbelianGroup could be relaxed to 
-- IsCommutative (or something similar)
commutative :: (IsAbelianGroup m, Different a, Rewrite a) => m a -> RewriteRule a
commutative m = rule m "commutative" $ 
   \x y -> x.y :~> y.x
 where 
   (.) = operation m
   
inverseDistribution :: (IsAbelianGroup m, Different a, Rewrite a) => m a -> RewriteRule a
inverseDistribution m = rule m "inverse.distribution" $ 
   \x y -> f (x.y) :~> f x.f y
 where 
   (.) = operation m
   f   = inverse m