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
module Service.BasicServices
   ( -- * Basic Services
     stepsremaining, findbuggyrules, ready, allfirsts, derivation
   , onefirst, applicable, allapplications, apply, generate
   ) where

import Common.Library hiding (derivation, applicable, apply, ready)
import Common.Utils (fst3)
import Data.List
import Data.Maybe
import Service.State
import System.Random (StdGen)
import qualified Common.Classes as Apply

generate :: StdGen -> Exercise a -> Difficulty -> State a
generate rng ex dif =
   emptyState ex (randomTermWith rng dif ex)

-- TODO: add a location to each step
derivation :: Maybe StrategyConfiguration -> State a -> Either String (Derivation (Rule (Context a), ArgValues) (Context a))
derivation mcfg state =
   mapSecond (biMap (\(r, _, as) -> (r, as)) stateContext) $
   case (statePrefix state, mcfg) of
      (Nothing, _) -> Left "Prefix is required"
      -- configuration is only allowed beforehand: hence, the prefix
      -- should be empty (or else, the configuration is ignored). This
      -- restriction should probably be relaxed later on.
      (Just p, Just cfg) | null (prefixToSteps p) ->
         let newStrategy = configure cfg (strategy ex)
             newExercise = ex {strategy = newStrategy}
         in rec timeout d0 (empyStateContext newExercise (stateContext state))
      _ -> rec timeout d0 state
 where
   d0 = emptyDerivation state
   ex = exercise state
   timeout = 50 :: Int

   rec i acc st =
      case onefirst st of
         Left _         -> Right acc
         Right (r, l, as, next)
            | i <= 0    -> Left msg
            | otherwise -> rec (i-1) (acc `extend` ((r, l, as), next)) next
    where
      msg = "Time out after " ++ show timeout ++ " steps. " ++
            show (biMap fst3 (prettyPrinterContext ex . stateContext) acc)

-- Note that we have to inspect the last step of the prefix afterwards, because
-- the remaining part of the derivation could consist of minor rules only.
allfirsts :: State a -> Either String [(Rule (Context a), Location, ArgValues, State a)]
allfirsts state =
   case statePrefix state of
      Nothing ->
         Left "Prefix is required"
      Just p0 ->
         let tree = cutOnStep (stop . lastStepInPrefix) (prefixTree p0 (stateContext state))
             f (r1, _, _, _) (r2, _, _, _) =
                ruleOrdering (exercise state) r1 r2
         in Right $ noDuplicates $ sortBy f $ mapMaybe make $ derivations tree
 where
   stop (Just (RuleStep r)) = isMajorRule r
   stop _ = False

   make d = do
      prefixEnd <- lastStep d
      let ca = lastTerm (withoutLast d)
      case lastStepInPrefix prefixEnd of
         Just (RuleStep r) | isMajorRule r -> return
            ( r
            , location (lastTerm d)
            , fromMaybe [] (expectedArguments r ca)
            , makeState (exercise state) (Just prefixEnd) (lastTerm d)
            )
         _ -> Nothing

   noDuplicates []     = []
   noDuplicates (x:xs) = x : noDuplicates (filter (not . eq x) xs)

   eq (r1, l1, a1, s1) (r2, l2, a2, s2) =
      r1==r2 && l1==l2 && a1==a2 && exercise s1 == exercise s2
      && similarity (exercise s1) (stateContext s1) (stateContext s2)

onefirst :: State a -> Either String (Rule (Context a), Location, ArgValues, State a)
onefirst state =
   case allfirsts state of
      Right []     -> Left "No step possible"
      Right (hd:_) -> Right hd
      Left msg     -> Left msg

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc state =
   let p r = not (isBuggyRule r) && Apply.applicable r (setLocation loc (stateContext state))
   in filter p (ruleset (exercise state))

allapplications :: State a -> [(Rule (Context a), Location, State a)]
allapplications state = sortBy cmp (xs ++ ys)
 where
   ex = exercise state
   xs = either (const []) (map (\(r, l, _, s) -> (r, l, s))) (allfirsts state)
   ps = [ (r, loc) | (r, loc, _) <- xs ]
   ys = maybe [] f (top (stateContext state))

   f c = g c ++ concatMap f (allDowns c)
   g c = [ (r, location new, makeState ex Nothing new)
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
apply :: Rule (Context a) -> Location -> State a -> Either String (State a)
apply r loc state = maybe applyOff applyOn (statePrefix state)
 where
   applyOn _ = -- scenario 1: on-strategy
      maybe applyOff Right $ listToMaybe
      [ s1 | Right xs <- [allfirsts state], (r1, loc1, _, s1) <- xs, r==r1, loc==loc1 ]

   applyOff  = -- scenario 2: off-strategy
      case Apply.apply r (setLocation loc (stateContext state)) of
         Just new -> Right (makeState (exercise state) Nothing new)
         Nothing  -> Left ("Cannot apply " ++ show r)

ready :: State a -> Bool
ready state = isReady (exercise state) (stateTerm state)

stepsremaining :: State a -> Either String Int
stepsremaining = mapSecond derivationLength . derivation Nothing

findbuggyrules :: State a -> a -> [(Rule (Context a), Location, ArgValues)]
findbuggyrules state a =
   [ (r, loc, as)
   | r         <- filter isBuggyRule (ruleset ex)
   , (loc, as) <- recognizeRule ex r (stateContext state) (inContext ex a)
   ]
 where
   ex = exercise state