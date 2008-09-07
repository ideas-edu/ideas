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
import Data.Maybe
{-
import Test.QuickCheck hiding (check)
import Data.List -}
{- generator
* max. 1 equivalentie
* min. 4 stappen (dus niet in DNF)
* geen T/F in formule
* max. ?? stappen
-}

dnfExercise :: Exercise Logic
dnfExercise = standard
   { shortTitle    = "Proposition to DNF" 
   , parser        = \s -> case parseLogicPars s of
                              (p, [])      -> Right (fromRanged p)
                              (_, (a,b):_) -> Left $ "Parse error" ++ 
                                              maybe "" (\x -> " on " ++ show x) b ++ ":\n   expecting " ++ show a
   , subTerm       = \s r -> case parseLogicPars s of
                                (p, []) -> fmap makeLocation (subExpressionAt r p)
                                _       -> Nothing
   , prettyPrinter = ppLogicPars . fromContext
   , equivalence   = \x y -> fromContext x `eqLogic` fromContext y
   , equality      = \x y -> fromContext x `equalLogicAC` fromContext y
   , finalProperty = isDNF . fromContext
   , ruleset       = map liftRuleToContext logicRules
   , strategy      = toDNFDWA
   , generator     = liftM inContext generateLogic
   , suitableTerm  = \p -> let n = stepsRemaining (emptyPrefix toDNF) p
                           in countEquivalences (fromContext p) < 2 && n >= 4 && n <= 12
   }
 where
   standard :: Exercise (Context Logic)
   standard = makeExercise
{-
r = makeRule "Test" $ 
   metaVar 0 :&&: metaVar 1 |- metaVar 1 :||: metaVar 0 -}

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
      return $ setLocation new c

foci = rec (inContext lhs1)
 where
   rec a = let n   = maybe 0 (length . children) (currentFocus a)
               is  = take n [0..]
               f i = changeLocation (locationDown i) a
           in a : concatMap (rec . f) is
           
interesting = catMaybes (map f foci)
 where
   f p = do 
      a <- currentFocus p
      s <- match a asso1
      return $ changeFocus (s |->) p

news = filter pr $ concatMap f interesting
 where
   f ma = let ys = applyAll rule1 (fromContext ma)
              xs = maybe [] (applyAll (asso1 |- asso2)) (currentFocus ma)
              zs = map (\x -> changeFocus (const x) ma) xs
          in [ (fromContext z, y) | z <- zs, y <- ys ]
          
pr (a, b) = generalizeAll (lhs1,rhs1) `implies` generalizeAll (a, b)
   -- maybe False (/=b1) (apply rule1 a1)
 where ((a1, b1), _) = instantiateWith substitutePair  777 $ generalizeAll (a, b) -}
 {-
implies :: Pat -> Pat -> Bool
implies p1 p2 = maybe False (==rhs2) (apply (lhs1 |- rhs1) lhs2)
 where
   ((lhs1, rhs1), i) = instantiateWith substitutePair 0 p1
   ((lhs2, rhs2), _) = instantiateWith substitutePair i p2
   
superImpose :: Pat -> Pat -> [Pat]
superImpose p1 p2 = result
 where
   ((lhs1, rhs1), i) = instantiateWith substitutePair 1001 p1 -- ??
   ((lhs2, rhs2), _) = instantiateWith substitutePair i p2
   
   foci = everywhere lhs1
   
   interesting = catMaybes (map consider foci)
   consider p = do 
      a <- currentFocus p
      s <- match a lhs2
      return $ changeFocus (s |->) p
      
   result = concatMap make interesting
   make ma = let ys = applyAll (lhs1 |- rhs1) (fromContext ma)
                 xs = maybe [] (applyAll (lhs2 |- rhs2)) (currentFocus ma)
                 zs = map (\x -> changeFocus (const x) ma) xs
             in [ generalizeAll (fromContext z, y) | z <- zs, y <- ys ]
      
superImposeNew :: Pat -> Pat -> [Pat]
superImposeNew p1 p2 = filter (not . (p1 `implies`)) $ superImpose p1 p2

deMorganPat = generalizeAll 
   ( Not (metaVar 0 :&&: metaVar 1)
   , Not (metaVar 0) :||: Not (metaVar 1)
   )
   
assocAndPat = generalizeAll
   ( (metaVar 0 :&&: metaVar 1) :&&: metaVar 2
   , metaVar 0 :&&: (metaVar 1 :&&: metaVar 2)
   )

assocAndPat2 = generalizeAll
   ( metaVar 0 :&&: (metaVar 1 :&&: metaVar 2)
   , (metaVar 0 :&&: metaVar 1) :&&: metaVar 2
   )

assocAndPatId = generalizeAll
   ( (metaVar 0 :&&: metaVar 1) :&&: metaVar 2
   , (metaVar 0 :&&: metaVar 1) :&&: metaVar 2
   )
   
commAndPat = generalizeAll
   ( metaVar 0 :&&: metaVar 1
   , metaVar 1 :&&: metaVar 0
   )
   
idemAndPat = generalizeAll
   ( metaVar 0 :&&: metaVar 0
   , metaVar 0
   )
   
[q1] = superImposeNew deMorganPat assocAndPat
[q2] = superImposeNew q1 assocAndPat
[q3] = superImposeNew q2 assocAndPat
[q4] = superImposeNew q3 assocAndPat 
   
q5 = superImpose assocAndPatId idemAndPat 
   
everywhere :: Uniplate a => a -> [Context a]
everywhere = rec . inContext 
 where
   rec a = let n   = maybe 0 (length . children) (currentFocus a)
               is  = take n [0..]
               f i = changeLocation (locationDown i) a
           in [ a | n > 0 ] ++ concatMap (rec . f) is
-}

{-
type Pat = ForAll (Logic, Logic)

test1 = testIdempOr $ Var "p" :||: Var "p"
test2 = testIdempOr $ (Var "p" :||: Var "q") :||: (Var "p" :||: Var "q")
test3 = testIdempOr $ Var "p" :||: Var "q" :||: Var "p" :||: Var "q"
test4 = testIdempOr $ Var "p" :||: (Var "p" :||: Var "q")
test5 = testIdempOr $ (Var "p" :||: Var "q") :||: ((Var "p" :||: Var "q") :||: Var "r")

testIdempOr p = apply (somewhere $ liftRuleToContext ruleIdempOr) (inContext p)
-}