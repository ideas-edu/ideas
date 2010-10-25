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
-- Group axioms specified as rewrite rules (directed).
--
-----------------------------------------------------------------------------
module Common.Rewriting.Axioms 
   ( -- Semigroup
     leftAssociative, rightAssociative, associative
     -- Monoid
   , leftIdentity, rightIdentity
     -- Group
   , leftInverse, rightInverse
   , inverseIdentity, inverseTwice
   , flippedInverseDistribution
     -- Abelian group
   , commutative, inverseDistribution
   ) where

import Common.Id
import Common.Rewriting.Group
import Common.Rewriting.RewriteRule

-- helper
rule :: (IsMagma m, IsId n, RuleBuilder f a, Rewrite a) => m a -> n -> f -> RewriteRule a
rule m s = rewriteRule (getId (toMagma m), s)

-------------------------------------------------------------------
-- * SemiGroup

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