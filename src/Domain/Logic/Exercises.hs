-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module Domain.Logic.Exercises where

import Domain.Logic.Generator
import Domain.Logic.Formula
import Domain.Logic.Strategies
import Domain.Logic.Parser
import Domain.Logic.Rules

import Common.Exercise
import Common.Strategy hiding (not)
import Common.Context
import Common.Parsing (fromRanged, subExpressionAt)
import Control.Monad

{- import Common.Transformation
import Common.Unification
import Common.Apply

import Test.QuickCheck hiding (check)
import Data.List -}
{- generator
* max. 1 equivalentie
* min. 4 stappen (dus niet in DNF)
* geen T/F in formule
* max. ?? stappen
-}

dnfExercise :: Exercise (Context Logic)
dnfExercise = standard
   { shortTitle    = "Proposition to DNF" 
   , parser        = \s -> case parseLogicPars s of
                              (p, [])      -> Right (inContext (fromRanged p))
                              (p, (a,b):_) -> Left $ text $ "Parse error" ++ 
                                              maybe "" (\x -> " on " ++ show x) b ++ ":\n   expecting " ++ show a
   , subTerm       = \s r -> case parseLogicPars s of
                                (p, []) -> fmap makeLocation (subExpressionAt r p)
                                _       -> Nothing
   , prettyPrinter = ppLogicPars . fromContext
   , equivalence   = \x y -> fromContext x `eqLogic` fromContext y
   , equality      = \x y -> fromContext x == fromContext y
   , finalProperty = isDNF . fromContext
   , ruleset       = map liftRuleToContext logicRules
   , strategy      = toDNF
   , generator     = liftM inContext generateLogic
   , suitableTerm  = \p -> let n = stepsRemaining (emptyPrefix toDNF) p
                           in countEquivalences (fromContext p) < 2 && n >= 4 && n <= 12
   }
 where
   standard :: Exercise (Context Logic)
   standard = makeExercise

{-
toNF :: Logic -> Logic
toNF (p :->: q) = toNF p :->: toNF q
toNF (p :<->: q) = toNF p :<->: toNF q
toNF (Not p) = Not (toNF p)
toNF ((p :&&: q) :&&: r) = toNF (p :&&: (q :&&: r))
toNF (p :&&: q) = toNF p :&&: toNF q
toNF ((p :||: q) :||: r) = toNF (p :||: (q :||: r))
toNF (p :||: q) = toNF p :||: toNF q
toNF x = x

variations :: Logic -> [Logic]
variations (p :->: q) = [ p1 :->: q1 | p1 <- variations p, q1 <- variations q ]
variations (p :<->: q) = [ p1 :<->: q1 | p1 <- variations p, q1 <- variations q ]
variations (Not p) = [ Not p1 | p1 <- variations p ]
variations p@(_ :&&: _) = concatMap (bins (:&&:)) (merge $ map variations $ conjunctions p)
variations p@(_ :||: _) = concatMap (bins (:||:)) (merge $ map variations $ disjunctions p)
variations x = [x]

merge :: [[Logic]] -> [[Logic]]
merge [] = [[]]
merge (xs:xss) = [ y:ys | y <- xs, ys <- merge xss ]

bins :: (a -> a -> a) -> [a] -> [a]
bins f []  = []
bins f [x] = [x]
bins f xs  = [ f y z | i <- [1 .. length xs-1], let (ys, zs) = splitAt i xs, y <- bins f ys, z <- bins f zs ]

prop1 :: Logic -> Bool
prop1 p = toNF p == toNF (toNF p) 

prop2 :: Logic -> Bool
prop2 p = length (nub (map toNF (variations p))) == 1

ex = Var "p" :&&: (Var "q" :&&: (Var "r" :&&: (Var "s" :&&: (Var "t" :&&: Var "u"))))
ex2 = ((Var "p" :&&: Var "q") :&&: (Var "r" :&&: Var "s")) :&&: (Var "t" :&&: Var "u")

q = map (toNF . fromContext) $ applyAll (somewhere (many (liftRuleToContext ruleAssocAndL) <*> liftRuleToContext ruleCommAnd)) (inContext ex)

ruleAssocAndL :: Rule Logic
ruleAssocAndL = makeSimpleRule "AssocAndL" f 
 where
   f (p :&&: (q :&&: r)) = return $ (p :&&: q) :&&: r
   f _ = Nothing

ruleAssocAndR :: Rule Logic
ruleAssocAndR = makeSimpleRule "AssocAndR" f 
 where
   f ((p :&&: q) :&&: r) = return $ p :&&: (q :&&: r)
   f _ = Nothing
      
exs = nub $ map (toNF . fromContext) $ concatMap f (variations ex)
 where f = applyAll (somewhere $ many (liftRuleToContext ruleAssocAndL) <*> liftRuleToContext ruleCommAnd) . inContext
 
assocStrat = fix $ \x -> 
   (many1 (liftRuleToContext ruleAssocAndL) <|> succeed <|> many1 (liftRuleToContext ruleAssocAndR)) <*> all2 x

-- | Apply a strategy on (exactly) one of the term's direct children
all2 :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
all2 s = (ruleMoveDown 0 <*> s <*> ruleMoveUp <*> ruleMoveDown 1 <*> s <*> ruleMoveUp) <|> check p
 where
   p c = maybe False (null . children) (currentFocus c)
   ruleMoveDown n = minorRule (makeSimpleRuleList "MoveDown" (moveDown n))
   moveDown i c = 
      let n = fmap (length . children) (currentFocus c)
      in [ changeLocation (locationDown i) c | n == Just 2 ]
   
   ruleMoveUp = minorRule (makeSimpleRule "MoveUp" moveUp)
   moveUp c   = do
      new <- locationUp (location c)
      return $ setLocation new c -}