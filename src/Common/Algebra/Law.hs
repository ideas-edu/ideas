{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
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
-----------------------------------------------------------------------------
module Common.Algebra.Law 
   ( Law, LawSpec((:==:)), law, mapLaw
   , propertyLaw, rewriteLaw
   ) where

import Test.QuickCheck
import Common.Rewriting.RewriteRule
import Common.Rewriting.Term

infix 1 :==:

data Law a = Law String (LawSpec a)

instance Show (Law a) where
   show (Law s _) = s

data LawSpec a = Abs (a -> LawSpec a) | a :==: a

law :: LawBuilder l a => String -> l -> Law a
law s l = Law s (lawSpec l)

class LawBuilder l a | l -> a where
   lawSpec :: l -> LawSpec a

instance LawBuilder (LawSpec a) a where
   lawSpec = id

instance LawBuilder b a => LawBuilder (a -> b) a where
   lawSpec f = Abs (lawSpec . f)
   
instance (Show a, Eq a, Arbitrary a) => Testable (Law a) where
   property = propertyLaw (==)

mapLaw :: (b -> a) -> (a -> b) -> Law a -> Law b
mapLaw to from (Law s l) = Law s (rec l)
 where
   rec (Abs f)    = Abs (rec . f . to)
   rec (a :==: b) = from a :==: from b

propertyLaw :: (Arbitrary a, Show a, Testable b) => (a -> a -> b) -> Law a -> Property
propertyLaw eq = rec . getLawSpec
 where
   rec (Abs f)    = property (rec . f)
   rec (a :==: b) = property (eq a b)

rewriteLaw :: (Rewrite a, Different a) => Law a -> RewriteRule a
rewriteLaw (Law s l) = rewriteRule s l

instance (IsTerm a, Different a) => RuleBuilder (LawSpec a) a where
   buildRuleSpec (a :==: b) = buildRuleSpec (a :~> b)
   buildRuleSpec (Abs f)    = buildRuleSpec f
   
getLawSpec :: Law a -> LawSpec a
getLawSpec (Law _ l) = l