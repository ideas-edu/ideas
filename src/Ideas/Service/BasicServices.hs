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
-----------------------------------------------------------------------------
module Ideas.Service.BasicServices
   ( -- * Basic Services
     stepsremaining, findbuggyrules, ready, allfirsts, derivation
   , onefirst, applicable, allapplications, apply, generate, generateWith
   , StepInfo
   ) where

import Ideas.Common.Library hiding (derivation, applicable, apply, ready)
import Ideas.Common.Traversal.Navigator (downs, navigateTo)
import Ideas.Common.Utils (fst3)
import Data.List
import Data.Maybe
import Ideas.Service.State
import System.Random
import Control.Monad
import qualified Ideas.Common.Classes as Apply

generate :: Exercise a -> Maybe Difficulty -> IO (State a)
generate ex = liftM (emptyState ex) . randomTerm ex

generateWith :: StdGen -> Exercise a -> Maybe Difficulty -> Either String (State a)
generateWith rng ex md = 
   case randomTermWith rng ex md of
      Just a  -> return (emptyState ex a)
      Nothing -> fail "No random term"

-- TODO: add a location to each step
derivation :: Maybe StrategyConfiguration -> State a -> Either String (Derivation (Rule (Context a), Environment) (Context a))
derivation mcfg state =
   mapSecond (biMap (\(r, _, as) -> (r, as)) stateContext) $
   case mcfg of
      _ | null ps -> Left "Prefix is required"
      -- configuration is only allowed beforehand: hence, the prefix
      -- should be empty (or else, the configuration is ignored). This
      -- restriction should probably be relaxed later on.
      Just cfg | all (null . prefixToSteps) ps ->
         let newStrategy = configure cfg (strategy ex)
             newExercise = ex {strategy = newStrategy}
         in rec timeout d0 (emptyStateContext newExercise (stateContext state))
      _ -> rec timeout d0 state
 where
   d0 = emptyDerivation state
   ex = exercise state
   ps = statePrefixes state
   timeout = 50 :: Int

   rec i acc st =
      case onefirst st of
         Left _         -> Right acc
         Right ((r, l, as), next)
            | i <= 0    -> Left msg
            | otherwise -> rec (i-1) (acc `extend` ((r, l, as), next)) next
    where
      msg = "Time out after " ++ show timeout ++ " steps. " ++
            show (biMap fst3 (prettyPrinterContext ex . stateContext) acc)

type StepInfo a = (Rule (Context a), Location, Environment) -- find a good place

-- Note that we have to inspect the last step of the prefix afterwards, because
-- the remaining part of the derivation could consist of minor rules only.
allfirsts :: State a -> Either String [(StepInfo a, State a)]
allfirsts state
   | null ps   = Left "Prefix is required"
   | otherwise =
        let trees  = map tree ps
            tree p = cutOnStep (stop . lastStepInPrefix) (prefixTree False p (stateContext state))
            f ((r1, _, _), _) ((r2, _, _), _) =
               ruleOrdering (exercise state) r1 r2
        in Right $ noDuplicates $ sortBy f $ mapMaybe make $ concatMap derivations trees
 where
   ps   = statePrefixes state
   stop = maybe False isMajor

   make d = do
      prefixEnd <- lastStep d
      case lastStepInPrefix prefixEnd of
         Just (RuleStep env r) | isMajor r -> return
            ( (r
              , location (lastTerm d)
              , env)
            , makeState (exercise state) [prefixEnd] (lastTerm d)
            )
         _ -> Nothing

   noDuplicates []     = []
   noDuplicates (x:xs) = x : noDuplicates (filter (not . eq x) xs)

   eq ((r1, l1, a1), s1) ((r2, l2, a2), s2) =
      r1==r2 && l1==l2 && a1==a2 && exercise s1 == exercise s2
      && similarity (exercise s1) (stateContext s1) (stateContext s2)

onefirst :: State a -> Either String (StepInfo a, State a)
onefirst state =
   case allfirsts state of
      Right []     -> Left "No step possible"
      Right (hd:_) -> Right hd
      Left msg     -> Left msg

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc state =
   let p r = not (isBuggy r) && Apply.applicable r (setLocation loc (stateContext state))
   in filter p (ruleset (exercise state))

allapplications :: State a -> [(Rule (Context a), Location, State a)]
allapplications state = sortBy cmp (xs ++ ys)
 where
   ex = exercise state
   xs = either (const []) (map (\((r, l, _), s) -> (r, l, s))) (allfirsts state)
   ps = [ (r, loc) | (r, loc, _) <- xs ]
   ys = f (top (stateContext state))

   f c = g c ++ concatMap f (downs c)
   g c = [ (r, location new, makeNoState ex new)
         | r   <- ruleset ex
         , (r, location c) `notElem` ps
         , new <- applyAll r c
         ]

   cmp (r1, loc1, _) (r2, loc2, _) =
      case ruleOrdering ex r1 r2 of
         EQ   -> loc1 `compare` loc2
         this -> this

-- local helper
setLocation :: Location -> Context a -> Context a
setLocation loc c0 = fromMaybe c0 (navigateTo loc c0)

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Rule (Context a) -> Location -> Environment -> State a -> Either String (State a)
apply r loc env state 
   | null (statePrefixes state) = applyOff
   | otherwise                  = applyOn
 where
   applyOn = -- scenario 1: on-strategy
      maybe applyOff Right $ listToMaybe
      [ s1 | Right xs <- [allfirsts state], ((r1, loc1, env1), s1) <- xs, r==r1, loc==loc1, noBindings env || env==env1 ]

   ca = setLocation loc (stateContext state)
   applyOff  = -- scenario 2: off-strategy
      case transApplyWith env (transformation r) ca of
         (new, _):_ -> Right (makeNoState (exercise state) new)
         []         -> Left ("Cannot apply " ++ show r)

ready :: State a -> Bool
ready state = isReady (exercise state) (stateTerm state)

stepsremaining :: State a -> Either String Int
stepsremaining = mapSecond derivationLength . derivation Nothing

findbuggyrules :: State a -> Context a -> [(Rule (Context a), Location, Environment)]
findbuggyrules state a =
   [ (r, loc, as)
   | r         <- filter isBuggy (ruleset ex)
   , (loc, as) <- recognizeRule ex r (stateContext state) a
   ]
 where
   ex = exercise state