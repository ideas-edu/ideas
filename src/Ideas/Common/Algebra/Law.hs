{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Common.Algebra.Law
   ( Law, LawSpec((:==:)), law, lawAbs, mapLaw
   , propertyLaw, rewriteLaw
   ) where

import Ideas.Common.Rewriting
import Test.QuickCheck

infix 1 :==:

data Law a = Law String (LawSpec a)

instance Show (Law a) where
   show (Law s _) = s

data LawSpec a
   = AbsMono (a -> LawSpec a) -- simple abstraction (fewer classes needed)
   | forall b . (Arbitrary b, Show b, Different b) => Abs (b -> LawSpec a) -- generalized abstraction
   | a :==: a

law :: LawBuilder l a => String -> l -> Law a
law s l = Law s (lawSpec l)

lawAbs :: (Different b, Arbitrary b, Show b) => (b -> LawSpec a) -> LawSpec a
lawAbs = Abs

class LawBuilder l a | l -> a where
   lawSpec :: l -> LawSpec a

instance LawBuilder (LawSpec a) a where
   lawSpec = id

instance LawBuilder (Law a) a where
   lawSpec = getLawSpec

instance LawBuilder b a => LawBuilder (a -> b) a where
   lawSpec f = AbsMono (lawSpec . f)

instance (Show a, Eq a, Arbitrary a) => Testable (Law a) where
   property = propertyLaw (==)

mapLaw :: (b -> a) -> (a -> b) -> Law a -> Law b
mapLaw to from (Law s l) = Law s (rec l)
 where
   rec (AbsMono f) = AbsMono (rec . f . to)
   rec (Abs f)     = Abs (rec . f)
   rec (a :==: b)  = from a :==: from b

propertyLaw :: (Arbitrary a, Show a, Testable b) => (a -> a -> b) -> Law a -> Property
propertyLaw eq = rec . getLawSpec
 where
   rec (AbsMono f) = property (rec . f)
   rec (Abs f)     = property (rec . f)
   rec (a :==: b)  = property (eq a b)

rewriteLaw :: (Different a, IsTerm a, Arbitrary a, Show a) => Law a -> RewriteRule a
rewriteLaw (Law s l) = makeRewriteRule s l

instance (Arbitrary a, IsTerm a, Show a, Different a) => RuleBuilder (LawSpec a) a where
   buildRuleSpec i (a :==: b)  = buildRuleSpec i (a :~> b)
   buildRuleSpec i (AbsMono f) = buildRuleSpec i f
   buildRuleSpec i (Abs f)     = buildRuleSpec i f

getLawSpec :: Law a -> LawSpec a
getLawSpec (Law _ l) = l