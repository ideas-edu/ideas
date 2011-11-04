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
-- Generalized rules, and inverse rules, for De Morgan and distributivity
--
-----------------------------------------------------------------------------
module Domain.Logic.GeneralizedRules
   ( generalRuleDeMorganOr, generalRuleDeMorganAnd
   , generalRuleAndOverOr, generalRuleOrOverAnd
   , inverseDeMorganOr, inverseDeMorganAnd
   , inverseAndOverOr, inverseOrOverAnd
   ) where

-- Note: the generalized rules do not take AC-unification into account,
-- and perhaps they should.
import Common.Algebra.Boolean
import Common.Rule (Rule)
import Control.Monad
import Domain.Logic.Formula
import qualified Common.Rule as Rule

makeSimpleRule :: String -> (a -> Maybe a) -> Rule a
makeSimpleRule s = Rule.makeSimpleRule ("logic.propositional." ++ s)

-----------------------------------------------------------------------------
-- Inverse rules

-- generalized (works for multiple terms)
inverseDeMorganOr :: Rule SLogic
inverseDeMorganOr = makeSimpleRule "InvDeMorganOr" $ \p -> do
   let xs = conjunctions p
   guard (length xs > 1)
   ys <- mapM isNot xs
   return (Not $ ors ys)

-- generalized (works for multiple terms)
inverseDeMorganAnd :: Rule SLogic
inverseDeMorganAnd = makeSimpleRule "InvDeMorganAnd" $ \p -> do
   let xs = disjunctions p
   guard (length xs > 1)
   ys <- mapM isNot xs
   return (Not $ ands ys)

inverseAndOverOr :: Rule SLogic
inverseAndOverOr = makeSimpleRule "InvAndOverOr" $ \p -> do
   let xs = disjunctions p
   guard (length xs > 1)
   do pairs <- mapM isAndHead xs
      let (as, ys) = unzip pairs
      guard (allSame as)
      return (head as :&&: ors ys)
    `mplus` do
      pairs <- mapM isAndLast xs
      let (ys, as) = unzip pairs
      guard (allSame as)
      return (ors ys :&&: head as)

inverseOrOverAnd :: Rule SLogic
inverseOrOverAnd = makeSimpleRule "InvOrOverAnd" $ \p -> do
   let xs = conjunctions p
   guard (length xs > 1)
   do pairs <- mapM isOrHead xs
      let (as, ys) = unzip pairs
      guard (allSame as)
      return (head as :||: ands ys)
    `mplus` do
      pairs <- mapM isOrLast xs
      let (ys, as) = unzip pairs
      guard (allSame as)
      return (ands ys :||: head as)

isNot :: SLogic -> Maybe SLogic
isNot (Not p) = Just p
isNot _       = Nothing

isAndHead, isAndLast, isOrHead, isOrLast :: SLogic -> Maybe (SLogic, SLogic)
isAndHead = useHead (:&&:) . conjunctions
isAndLast = useLast (:&&:) . conjunctions
isOrHead  = useHead (:||:) . disjunctions
isOrLast  = useLast (:||:) . disjunctions

useHead, useLast :: (a -> a -> a) -> [a] -> Maybe (a, a)
useHead op (x:xs) | not (null xs) =
   Just (x, foldr1 op xs)
useHead _ _ = Nothing

useLast op = fmap (\(x, y) -> (y, x)) . useHead (flip op) . reverse

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (==x) xs

-----------------------------------------------------------------------------
-- Generalized rules

generalRuleDeMorganOr :: Rule SLogic
generalRuleDeMorganOr = makeSimpleRule "GenDeMorganOr" f
 where
   f (Not e) = do
      let xs = disjunctions e
      guard (length xs > 2)
      return (ands (map Not xs))
   f _ = Nothing

generalRuleDeMorganAnd :: Rule SLogic
generalRuleDeMorganAnd = makeSimpleRule "GenDeMorganAnd" f
 where
   f (Not e) = do
      let xs = conjunctions e
      guard (length xs > 2)
      return (ors (map Not xs))
   f _ = Nothing

generalRuleAndOverOr :: Rule SLogic
generalRuleAndOverOr = makeSimpleRule "GenAndOverOr" f
 where
   f (x :&&: y) =
      case (disjunctions x, disjunctions y) of
         (xs, _) | length xs > 2 ->
            return (ors (map (:&&: y) xs))
         (_, ys) | length ys > 2 ->
            return (ors (map (x :&&:) ys))
         _ -> Nothing
   f _ = Nothing

generalRuleOrOverAnd :: Rule SLogic
generalRuleOrOverAnd = makeSimpleRule "GenOrOverAnd" f
 where
   f (x :||: y) =
      case (conjunctions x, conjunctions y) of
         (xs, _) | length xs > 2 ->
            return (ands (map (:||: y) xs))
         (_, ys) | length ys > 2 ->
            return (ands (map (x :||:) ys))
         _ -> Nothing
   f _ = Nothing