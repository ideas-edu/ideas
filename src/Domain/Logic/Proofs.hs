{-# LANGUAGE RankNTypes #-}
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
-- Exercise for the logic domain: to prove two propositions equivalent
--
-----------------------------------------------------------------------------
module Domain.Logic.Proofs 
   ( proofExercise, proofUnicodeExercise
   ) where

import Common.Algebra.Boolean
import Common.Algebra.CoBoolean
import Common.Library
import Common.Rewriting.AC
import Common.Utils
import Common.Utils.Uniplate
import Control.Monad
import Data.Foldable (toList)
import Data.List
import qualified Data.Set as S
import Domain.Logic.BuggyRules
import Domain.Logic.Examples
import Domain.Logic.Formula
import Domain.Logic.GeneralizedRules
import Domain.Logic.Generator (equalLogicA)
import Domain.Logic.Parser
import Domain.Logic.Rules
import Domain.Logic.Strategies (somewhereOr)
import Domain.Math.Expr ()

see :: Int -> IO ()
see n = printDerivation proofExercise (snd (examples proofExercise !! n))

-- Currently, we use the DWA strategy
proofExercise :: Exercise Proof
proofExercise = makeExercise
   { exerciseId     = describe "Prove two propositions equivalent" $
                         newId "logic.proof"
   , status         = Experimental
   , parser         = mapSecond makeProof . parseLogicProof False
   , prettyPrinter  = showProof
   , equivalence    = withoutContext equivalentProofs
   , similarity     = withoutContext similarProofs
   , suitable       = predicate $ all (uncurry eqLogic) . subProofs
   , ready          = predicate $ all (uncurry equalLogicA) . subProofs
   , strategy       = proofStrategy
   , extraRules     = map use (extraLogicRules ++ buggyRules)
   , navigation     = termNavigator
   , examples       = map (\a -> (Easy, makeProof a)) $
                      let p = Var (ShowString "p")
                          q = Var (ShowString "q")
                      in exampleProofs ++ [(q :&&: p, p :&&: (q :||: q))]
   }

proofUnicodeExercise :: Exercise Proof
proofUnicodeExercise = proofExercise 
   { exerciseId    = describe "Prove two propositions equivalent (unicode support)" $
                        newId "logic.propositional.proof.unicode"
   , parser        = mapSecond makeProof . parseLogicProof True
   , prettyPrinter = showProofUnicode
   }

type Proof = Logic (SLogic, SLogic)

subProofs :: Proof -> [(SLogic, SLogic)]
subProofs = toList

makeProof :: (SLogic, SLogic) -> Proof
makeProof = Var

proofPair :: Proof -> (SLogic, SLogic)
proofPair x = (catLogic (fmap fst x), catLogic (fmap snd x))

showProof :: Proof -> String
showProof = uncurry f . proofPair
 where
   f p q = ppLogicPars p ++ " == " ++ ppLogicPars q

showProofUnicode :: Proof -> String
showProofUnicode = uncurry f . proofPair
 where
   f p q = ppLogicUnicodePars p ++ " == " ++ ppLogicUnicodePars q

equivalentProofs :: Proof -> Proof -> Bool
equivalentProofs proof1 proof2 =
   let (p1, q1) = proofPair proof1
       (p2, q2) = proofPair proof2
   in eqLogic p1 p2 && eqLogic q1 q2

similarProofs :: Proof -> Proof -> Bool
similarProofs proof1 proof2 =
   let (p1, q1) = proofPair proof1
       (p2, q2) = proofPair proof2
   in equalLogicA p1 p2 && equalLogicA q1 q2

proofStrategy :: LabeledStrategy (Context Proof)
proofStrategy = label "proof equivalent" $
   repeatS (
         somewhere splitTop
      |> somewhere (useC commonExprAtom)
      |> somewhere rest
      ) <*>
   repeatS (somewhere (use normLogicRule))
 where
   splitTop =  use topIsNot  <|> use topIsAnd <|> use topIsOr
           <|> use topIsImpl <|> use topIsEquiv
   rest =  use notDNF <*> useC (repeatS dnfStrategyDWA)
       <|> simpler

   simpler :: Strategy (Context Proof)
   simpler =
      use tautologyOr <|> use idempotencyAnd <|> use contradictionAnd
      <|> use absorptionSubset <|> use fakeAbsorption <|> use fakeAbsorptionNot
      <|> alternatives (map use list)

   list = [ ruleFalseZeroOr, ruleTrueZeroOr, ruleIdempOr
          , ruleAbsorpOr, ruleComplOr
          ]

   notDNF :: Rule SLogic
   notDNF = minor $ makeRule "not-dnf" $ \p ->
      if isDNF p then Nothing else Just p

-----------------------------------------------------------------------------
-- To DNF, with priorities (the "DWA" approach)

dnfStrategyDWA :: Strategy (Context SLogic)
dnfStrategyDWA =
   toplevel <|> somewhereOr
      (  label "Simplify"                            simplify
      |> label "Eliminate implications/equivalences" eliminateImplEquiv
      |> label "Eliminate nots"                      eliminateNots
      |> label "Move ors to top"                     orToTop
      )
 where
    toplevel = useRules
       [ ruleFalseZeroOr, ruleTrueZeroOr, ruleIdempOr
       , ruleAbsorpOr, ruleComplOr
       ]
    simplify = somewhere $ useRules
       [ ruleFalseZeroOr, ruleTrueZeroOr, ruleTrueZeroAnd
       , ruleFalseZeroAnd, ruleNotTrue, ruleNotFalse
       , ruleNotNot, ruleIdempOr, ruleIdempAnd, ruleAbsorpOr, ruleAbsorpAnd
       , ruleComplOr, ruleComplAnd
       ]
    eliminateImplEquiv = somewhere $ useRules
       [ ruleDefImpl, ruleDefEquiv
       ]
    eliminateNots = somewhere $ useRules
       [ generalRuleDeMorganAnd, generalRuleDeMorganOr
       , ruleDeMorganAnd, ruleDeMorganOr
       ]
    orToTop = somewhere $ useRules
       [ generalRuleAndOverOr, ruleAndOverOr ]

useRules :: [Rule SLogic] -> Strategy (Context SLogic)
useRules = alternatives . map liftToContext

{-
onceLeft :: IsStrategy f => f (Context a) -> Strategy (Context a)
onceLeft s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule "MoveDown" (downTo 1)
   ruleMoveUp   = minorRule "MoveUp" safeUp

   safeUp a = Just (fromMaybe a (up a))

onceRight :: IsStrategy f => f (Context a) -> Strategy (Context a)
onceRight s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minor $ makeRule "MoveDown" (downTo 2)
   ruleMoveUp   = minorRule "MoveUp" safeUp

   safeUp a = Just (fromMaybe a (up a)) -}

normLogicRule :: Rule (SLogic, SLogic)
normLogicRule = ruleMaybe "Normalize" $ \tuple@(p, q) -> do
   guard (p /= q)
   let xs  = sort (varsLogic p `union` varsLogic q)
       new = (normLogicWith xs p, normLogicWith xs q)
   guard (tuple /= new)
   return new

-- disabled for now

-- Find a common subexpression that can be treated as a box
commonExprAtom :: Rule (Context (SLogic, SLogic))
commonExprAtom = minor $ ruleTrans "commonExprAtom" $ makeTransLiftContext $ \(p, q) -> do
   let xs = filter (same <&&> complement isAtomic) (largestCommonSubExpr p q) 
       same cse = eqLogic (sub cse p) (sub cse q)
       new = head (logicVars \\ (varsLogic p `union` varsLogic q))
       sub a this
          | a == this = Var new
          | otherwise = descend (sub a) this
   case xs of
      hd:_ -> do
         xs <- substRef :? []
         substRef := (show new, show hd):xs
         return (sub hd p, sub hd q)
      _ -> fail "not applicable"

largestCommonSubExpr :: (Uniplate a, Ord a) => a -> a -> [a]
largestCommonSubExpr x = rec 
 where
   uniX  = S.fromList (universe x)
   rec y | y `S.member` uniX = [y]
         | otherwise         = concatMap rec (children y)

substRef :: Ref [(String, String)]
substRef = makeRef "subst"

logicVars :: [ShowString]
logicVars = [ ShowString [c] | c <- ['a'..] ]

normLogic :: Ord a => Logic a -> Logic a
normLogic p = normLogicWith (sort (varsLogic p)) p

normLogicWith :: Eq a => [a] -> Logic a -> Logic a
normLogicWith xs p = make (filter keep (subsets xs))
 where
   keep ys = evalLogic (`elem` ys) p
   make = ors . map atoms
   atoms ys = ands [ f (x `elem` ys) (Var x) | x <- xs ]
   f b = if b then id else Not

-- p \/ q \/ ~p     ~> T           (propageren)
tautologyOr :: Rule SLogic
tautologyOr = ruleMaybe "tautologyOr" $ \p -> do
   let xs = disjunctions p
   guard (any (\x -> Not x `elem` xs) xs)
   return T

-- p /\ q /\ p      ~> p /\ q
idempotencyAnd :: Rule SLogic
idempotencyAnd = ruleMaybe "idempotencyAnd" $ \p -> do
   let xs = conjunctions p
       ys = nub xs
   guard (length ys < length xs)
   return (ands ys)

-- p /\ q /\ ~p     ~> F           (propageren)
contradictionAnd :: Rule SLogic
contradictionAnd = ruleMaybe "contradictionAnd" $ \p -> do
   let xs = conjunctions p
   guard (any (\x -> Not x `elem` xs) xs)
   return F

-- (p /\ q) \/ ... \/ (p /\ q /\ r)    ~> (p /\ q) \/ ...
--    (subset relatie tussen rijtjes: bijzonder geval is gelijke rijtjes)
absorptionSubset :: Rule SLogic
absorptionSubset = ruleList "absorptionSubset" $ \p -> do
   let xss = map conjunctions (disjunctions p)
       yss = nub $ filter (\xs -> all (ok xs) xss) xss
       ok xs ys = not (ys `isSubsetOf` xs) || xs == ys
   guard (length yss < length xss)
   return $ ors (map ands yss)

-- p \/ ... \/ (~p /\ q /\ r)  ~> p \/ ... \/ (q /\ r)
--    (p is hier een losse variabele)
fakeAbsorption :: Rule SLogic
fakeAbsorption = makeRule "fakeAbsorption" $ \p -> do
   let xs = disjunctions p
   v <- [ a | a@(Var _) <- xs ]
   let ys  = map (ands . filter (/= Not v) . conjunctions) xs
       new = ors ys
   guard (p /= new)
   return new

-- ~p \/ ... \/ (p /\ q /\ r)  ~> ~p \/ ... \/ (q /\ r)
--   (p is hier een losse variabele)
fakeAbsorptionNot :: Rule SLogic
fakeAbsorptionNot = makeRule "fakeAbsorptionNot" $ \p -> do
   let xs = disjunctions p
   v <- [ a | Not a@(Var _) <- xs ]
   let ys  = map (ands . filter (/= v) . conjunctions) xs
       new = ors ys
   guard (p /= new)
   return new

topIsNot :: Rule Proof
topIsNot = minorRule "top-is-not" f
 where
   f (Var (Not p, Not q)) = Just (Not (Var (p, q)))
   f _ = Nothing

acTopRuleFor :: IsId s => s -> (forall a . View (Logic a) (Logic a, Logic a)) -> Rule Proof
acTopRuleFor s v = minorRule s f
 where
   f (Var (lhs, rhs)) = do
      let collect a = maybe [a] (\(x, y) -> collect x ++ collect y) (match v a)
          make    = foldr1 (curry (build v))
          xs = collect lhs
          ys = collect rhs
      guard (length xs > 1 && length ys > 1)
      list <- liftM (map (make *** make)) (pairingsA False xs ys)
      guard (all (uncurry eqLogic) list)
      return (foldr1 (curry (build v)) (map Var list))
   f _ = []

topIsAnd :: Rule Proof
topIsAnd = acTopRuleFor "top-is-and" (makeView isAnd (uncurry (<&&>)))

topIsOr :: Rule Proof
topIsOr = acTopRuleFor "top-is-or" (makeView isOr (uncurry (<||>)))

topIsEquiv :: Rule Proof
topIsEquiv = acTopRuleFor "top-is-equiv" (makeView isEquiv (uncurry equivalent))
 where
   isEquiv (p :<->: q) = Just (p, q)
   isEquiv _           = Nothing

topIsImpl :: Rule Proof
topIsImpl = minorRule "top-is-impl" f
 where
   f (Var (p :->: q, r :->: s)) = do
      guard (eqLogic p r && eqLogic q s)
      return (Var (p, r) :->: Var (q, s))
   f _ = Nothing

{- Strategie voor sterke(?) normalisatie

(prioritering)

1. p \/ q \/ ~p     ~> T           (propageren)
   p /\ q /\ p      ~> p /\ q
   p /\ q /\ ~p     ~> F           (propageren)

2. (p /\ q) \/ ... \/ (p /\ q /\ r)    ~> (p /\ q) \/ ...
         (subset relatie tussen rijtjes: bijzonder geval is gelijke rijtjes)
   p \/ ... \/ (~p /\ q /\ r)  ~> p \/ ... \/ (q /\ r)
         (p is hier een losse variabele)
   ~p \/ ... \/ (p /\ q /\ r)  ~> ~p \/ ... \/ (q /\ r)
         (p is hier een losse variabele)

3. a) elimineren wat aan een kant helemaal niet voorkomt (zie regel hieronder)
   b) rijtjes sorteren
   c) rijtjes aanvullen

Twijfelachtige regel bij stap 3: samennemen in plaats van aanvullen:
   (p /\ q /\ r) \/ ... \/ (~p /\ q /\ r)   ~> q /\ r
          (p is hier een losse variable)
-}