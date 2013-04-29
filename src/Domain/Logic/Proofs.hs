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
import Data.Function (on)
import Data.Maybe
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
import Common.Traversal.Navigator
import Common.Traversal.Utils

see :: Int -> IO ()
see n = do
   let a   = snd (examples proofExercise !! n)
       der = defaultDerivation proofExercise a
   printDerivation proofExercise a
   putStrLn $ ">> " ++ show (derivationLength der) ++ " steps\n"

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
   , examples       = map (\(d, a) -> (d, makeProof a)) exampleProofs
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
      -- somewhere (useC commonExprAtom)   -- (tijdelijk uitgezet)
      |> somewhere rest
      )  
     <*> normStrategy
 where
   rest =  use notDNF <*> useC (repeatS dnfStrategyDWA)
       <|> simpler

   simpler :: Strategy (Context Proof)
   simpler = alternatives $ map use
      [ tautologyOr, idempotencyOr, idempotencyAnd, contradictionAnd
      , ruleAbsorpOrNot, ruleAbsorpAndNot
      , ruleFalseZeroOr, ruleTrueZeroOr, ruleIdempOr
      , ruleAbsorpOr, ruleComplOr
      ]

   notDNF :: Rule SLogic
   notDNF = minor $ makeRule "not-dnf" $ \p ->
      if isDNF p then Nothing else Just p

splitTop =  use topIsNot  <|> use topIsAnd <|> use topIsOr
           <|> use topIsImpl <|> use topIsEquiv <|>
               use topIsAndCom <|> use topIsOrCom <|> use topIsEquivCom

normStrategy :: Strategy (Context Proof)
normStrategy = repeatS $
   somewhere (use ruleFalseZeroAnd <|> use ruleTrueZeroOr)
   |>
   somewhere (use ruleComplAnd)
   |>
   somewhere (
      use sortRuleAnd <|> 
      use ruleIdempOr <|> 
      use ruleIdempAnd <|> 
      use ruleAndOverOr <|>
      use ruleFalseZeroOr
      )
   |> 
   somewhere (use sortRuleOr)
   |>
   somewhereDisjunct introduceVar
 
       
--introStrat :: Strategy SLogic
--introStrat = check missing <*> use introTrueLeft <*> layer [] introCompl

sortRuleBy :: (b -> b -> Ordering) -> View a [b] -> Transformation a
sortRuleBy cmp v = makeTrans $ \p -> do
   xs <- match v p
   guard (not (sortedBy cmp xs))
   let ys = sortBy cmp xs
   return (build v ys)
   
sortRuleOr :: Rule SLogic
sortRuleOr = ruleTrans "CommOr.sort" $ 
   sortRuleBy compareVar $ disjunctions <-> ors
   
sortRuleAnd :: Rule SLogic
sortRuleAnd = ruleTrans "CommAnd.sort" $ 
   sortRuleBy compareVar $ conjunctions <-> ands

compareVar :: Ord a => Logic a -> Logic a -> Ordering
compareVar = compare `on` (\x -> (varsLogic x, x))

sortedBy :: (a -> a -> Ordering) -> [a] -> Bool
sortedBy cmp = rec 
 where
   rec (x:y:zs) = cmp x y /= GT && rec (y:zs)
   rec _        = True 

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
normLogicRule :: Rule (SLogic, SLogic)
normLogicRule = ruleMaybe "Normalize" $ \tuple@(p, q) -> do
   guard (p /= q)
   let xs  = sort (varsLogic p `union` varsLogic q)
       new = (normLogicWith xs p, normLogicWith xs q)
   guard (tuple /= new)
   return new -}

-- disabled for now

-- Find a common subexpression that can be treated as a box
{-
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
-}

{-
normLogic :: Ord a => Logic a -> Logic a
normLogic p = normLogicWith (sort (varsLogic p)) p

normLogicWith :: Eq a => [a] -> Logic a -> Logic a
normLogicWith xs p = make (filter keep (subsets xs))
 where
   keep ys = evalLogic (`elem` ys) p
   make = ors . map atoms
   atoms ys = ands [ f (x `elem` ys) (Var x) | x <- xs ]
   f b = if b then id else Not
-}

-- p \/ q \/ ~p  ~>  reorder p and ~p 
tautologyOr :: Rule SLogic
tautologyOr = ruleMaybe "tautology-or.sort" $ \p -> do
   let xs = disjunctions p
       ys = sortBy compareVar xs
   guard (xs /= ys && any (\x -> Not x `elem` xs) xs)
   return (ors ys)

-- p /\ q /\ ~p  ~>  reorder p and ~p 
contradictionAnd :: Rule SLogic
contradictionAnd = ruleMaybe "contradiction-and.sort" $ \p -> do
   let xs = conjunctions p
       ys = sortBy compareVar xs
   guard (xs /= ys && any (\x -> Not x `elem` xs) xs)
   return (ands ys)

-- p \/ q \/ p      ~> reorder p's
idempotencyOr :: Rule SLogic
idempotencyOr = ruleMaybe "idempotency-or.sort" $ \p -> do
   let xs = disjunctions p
       ys = sortBy compareVar xs
   guard (xs /= ys && not (distinct xs))
   return (ors ys)

-- p /\ q /\ p      ~> reorder p's
idempotencyAnd :: Rule SLogic
idempotencyAnd = ruleMaybe "idempotency-and.sort" $ \p -> do
   let xs = conjunctions p
       ys = sortBy compareVar xs
   guard (xs /= ys && not (distinct xs))
   return (ands ys)

{-
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
   return new -}
   
acTopRuleFor :: Bool -> (forall a . Isomorphism (Logic a) [Logic a])
             -> Transformation Proof
acTopRuleFor com ep = makeTrans $ \proof -> do
   pair <- maybeToList (getSingleton proof)
   let pairings = if com then pairingsAC else pairingsA
       ep2 = ep *** ep
       (xs, ys) = from ep2 pair
   guard (length xs > 1 && length ys > 1)
   list <- liftM (map (to ep2)) (pairings False xs ys)
   guard (all (uncurry eqLogic) list)
   return (to ep (map Var list))

collect :: View a (a, a) -> Isomorphism a [a]
collect v = f <-> g
 where
   f x = maybe [x] (\(y, z) -> f y ++ f z) (match v x)
   g   = foldr1 (curry (build v))

andView, orView, eqView :: View (Logic a) (Logic a, Logic a)
andView = makeView isAnd (uncurry (<&&>))
orView  = makeView isOr  (uncurry (<||>))
eqView  = makeView isEq  (uncurry equivalent)
 where 
   isEq (p :<->: q) = Just (p, q)
   isEq _           = Nothing

topIsAnd :: Rule Proof
topIsAnd = minor $ ruleTrans "top-is-and" $ acTopRuleFor False (collect andView)

topIsOr :: Rule Proof
topIsOr = minor $ ruleTrans "top-is-or" $ acTopRuleFor False (collect orView)

topIsEquiv :: Rule Proof
topIsEquiv = minor $ ruleTrans "top-is-equiv"  $ acTopRuleFor False (collect eqView)

topIsAndCom :: Rule Proof
topIsAndCom = ruleTrans "top-is-and.com" $ acTopRuleFor True (collect andView)

topIsOrCom :: Rule Proof
topIsOrCom = ruleTrans "top-is-or.com" $ acTopRuleFor True (collect orView)

topIsEquivCom :: Rule Proof
topIsEquivCom = ruleTrans "top-is-equiv.com"  $ acTopRuleFor True (collect eqView)

topIsImpl :: Rule Proof
topIsImpl = minorRule "top-is-impl" f
 where
   f (Var (p :->: q, r :->: s)) = do
      guard (eqLogic p r && eqLogic q s)
      return (Var (p, r) :->: Var (q, s))
   f _ = Nothing

topIsNot :: Rule Proof
topIsNot = minorRule "top-is-not" f
 where
   f (Var (Not p, Not q)) = Just (Not (Var (p, q)))
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

-----------------------------------------------
-- Introduction of var

introduceVar :: Strategy (Context Proof)
introduceVar =  check missing
            <*> use introTrueLeft 
            <*> layer [] introCompl

missing :: Context Proof -> Bool
missing = isJust . missingVar

localEqVars :: Context Proof -> [ShowString]
localEqVars cp =
   case currentTerm cp >>= fromTerm of
      Just (p, q) -> varsLogic p `union` varsLogic q
      Nothing     -> maybe [] localEqVars (up cp)

missingVar :: Context Proof -> Maybe ShowString
missingVar cp = 
   case currentTerm cp >>= fromTerm of
      Just p  -> listToMaybe (localEqVars cp \\ varsLogic p)
      Nothing -> Nothing
  
introTrueLeft :: Rule SLogic
introTrueLeft = rewriteRule "IntroTrueLeft" $
   \x -> x  :~>  T :&&: x
   
introCompl :: Rule (Context Proof)
introCompl = makeRule "IntroCompl" $ \cp -> do
   a <- missingVar (safe up cp)
   let f = fromTerm >=> fmap toTerm . introTautology a
   changeTerm f cp 
 where
   introTautology :: a -> Logic a -> Maybe (Logic a)
   introTautology a T = Just (Var a :||: Not (Var a))
   introTautology _ _ = Nothing
 
 
 {-
go = applyAll (somewhereDisjunct introduceVar) $ inContext proofExercise $ 
   makeProof (p :||: (Not p :&&: q), p :||: q)
 where 
   p = Var (ShowString "p")
   q = Var (ShowString "q") 
   
somewhereEq :: IsStrategy f => f (Context Proof) -> Strategy (Context Proof)
somewhereEq s = traverse [once, topdown] 
   (check isEq <*> layer [] s)
 where
   isEq :: Context Proof -> Bool
   isEq cp = fromMaybe False $ do
      t <- currentTerm cp
      case fromTerm t :: Maybe (SLogic, SLogic) of
         Just (p, q) -> return True
         _           -> return False -}
   
somewhereDisjunct :: IsStrategy f => f (Context Proof) -> Strategy (Context Proof)
somewhereDisjunct s = oncetd (check isEq <*> layer [] (somewhereOrG s))
 where
   isEq :: Context Proof -> Bool
   isEq cp = (isJust :: Maybe (SLogic, SLogic) -> Bool)
             (currentTerm cp >>= fromTerm :: Maybe (SLogic, SLogic))

somewhereOrG :: IsStrategy g => g (Context a) -> Strategy (Context a)
somewhereOrG s =
   let isOr a = case currentTerm a >>= (fromTerm :: Term -> Maybe SLogic) of
                   Just (_ :||: _) -> True
                   _               -> False
   in fix $ \this -> check (Prelude.not . isOr) <*> s
                 <|> check isOr <*> layer [] this
                 
----------------------

ruleAbsorpOrNot :: Rule SLogic
ruleAbsorpOrNot = rewriteRules "AbsorpOrNot.distr"
   [ -- not inside
     \x y -> x :||: (Not x :&&: y)  :~>  (x :||: Not x) :&&: (x :||: y)
   , \x y -> x :||: (y :&&: Not x)  :~>  (x :||: y) :&&: (x :||: Not x)
   , \x y -> (Not x :&&: y) :||: x  :~>  (Not x :||: x) :&&: (y :||: x)
   , \x y -> (y :&&: Not x) :||: x  :~>  (y :||: x) :&&: (Not x :||: x)
     -- not outside
   , \x y -> Not x :||: (x :&&: y)  :~>  (Not x :||: x) :&&: (Not x :||: y)
   , \x y -> Not x :||: (y :&&: x)  :~>  (Not x :||: y) :&&: (Not x :||: x)
   , \x y -> (x :&&: y) :||: Not x  :~>  (x :||: Not x) :&&: (y :||: Not x)
   , \x y -> (y :&&: x) :||: Not x  :~>  (y :||: Not x) :&&: (x :||: Not x)
   ]
   
ruleAbsorpAndNot :: Rule SLogic
ruleAbsorpAndNot = rewriteRules "AbsorpAndNot.distr"
   [ -- not inside
     \x y -> x :&&: (Not x :||: y)  :~>  (x :&&: Not x) :||: (x :&&: y)
   , \x y -> x :&&: (y :||: Not x)  :~>  (x :&&: y) :||: (x :&&: Not x)
   , \x y -> (Not x :||: y) :&&: x  :~>  (Not x :&&: x) :||: (y :&&: x)
   , \x y -> (y :||: Not x) :&&: x  :~>  (y :&&: x) :||: (Not x :&&: x)
     -- not outside
   , \x y -> Not x :&&: (x :||: y)  :~>  (Not x :||: x) :&&: (Not x :||: y)
   , \x y -> Not x :&&: (y :||: x)  :~>  (Not x :||: y) :&&: (Not x :||: x)
   , \x y -> (x :||: y) :&&: Not x  :~>  (x :||: Not x) :&&: (y :||: Not x)
   , \x y -> (y :||: x) :&&: Not x  :~>  (y :||: Not x) :&&: (x :||: Not x)
   ]