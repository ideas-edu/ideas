-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Service.BasicServices
   ( -- * Basic Services
     stepsremaining, findbuggyrules, allfirsts, solution, solutionMaxSteps
   , onefirst, onefinal, applicable, allapplications, generate, create
   , StepInfo, tStepInfo, exampleDerivations, recognizeRule
   ) where

import Control.Monad
import Data.List
import Ideas.Common.Examples
import Ideas.Common.Library hiding (applicable, apply, ready)
import Ideas.Common.Traversal.Navigator (downs)
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Utils.Prelude (fst3)
import Test.QuickCheck.Random
import qualified Ideas.Common.Classes as Apply
import qualified Ideas.Common.Library as Library

generate :: QCGen -> Exercise a -> Maybe Difficulty -> Maybe String -> Either String (State a)
generate rng ex md userId =
   case randomTerm rng ex md of
      Just a  -> Right $ startState rng ex userId a
      Nothing -> Left "No random term"

create :: QCGen -> Exercise a -> Context a -> Maybe String -> Either String (State a)
create rng ex ctx userId =
   case fromContext ctx of
      Just a 
         | evalPredicate (Library.ready ex) a    -> Left "Is ready"
         | evalPredicate (Library.suitable ex) a -> Right $ startStateContext rng ex userId ctx
         | otherwise -> Left "Not suitable"
      Nothing -> Left "Invalid value"

-- TODO: add a location to each step
solution :: Maybe StrategyCfg -> State a -> Either String (Derivation (StepInfo a) (Context a))
solution = solutionMaxSteps 50

solutionMaxSteps :: Int -> Maybe StrategyCfg -> State a -> Either String (Derivation (StepInfo a) (Context a))
solutionMaxSteps maxSteps mcfg state =
   mapSecond (fmap stateContext) $
   case mcfg of
      _ | withoutPrefix state -> Left "Prefix is required"
      -- configuration is only allowed beforehand: hence, the prefix
      -- should be empty (or else, the configuration is ignored). This
      -- restriction should probably be relaxed later on.
      Just cfg | isEmptyPrefix prfx ->
         let newStrategy = configure cfg (strategy ex)
             newPrefix   = emptyPrefix newStrategy (stateContext state)
         in rec maxSteps d0 state { statePrefix = newPrefix }
      _ -> rec maxSteps d0 state
 where
   d0   = emptyDerivation state
   ex   = exercise state
   prfx = statePrefix state

   rec i acc st =
      case onefirst st of
         Left _         -> Right acc
         Right ((r, l, as), newState)
            | i <= 0    -> Left msg
            | otherwise -> rec (i-1) (acc `extend` ((r, l, as), newState)) newState
    where
      msg = "Time out after " ++ show maxSteps ++ " steps. " ++
            show (biMap fst3 (prettyPrinterContext ex . stateContext) acc)

type StepInfo a = (Rule (Context a), Location, Environment) -- find a good place

tStepInfo :: Type a (StepInfo a)
tStepInfo = Tag "step" (tTuple3 tRule tLocation tEnvironment)

allfirsts :: State a -> Either String [(StepInfo a, State a)]
allfirsts state
   | withoutPrefix state = Left "Prefix is required"
   | otherwise = Right $
        noDuplicates $ map make $ firsts state
 where
   make ((s, ctx, env), st) = ((s, location ctx, env), st)

   noDuplicates []     = []
   noDuplicates (x:xs) = x : noDuplicates (filter (not . eq x) xs)

   eq (x1, s1) (x2, s2) =
      x1 == x2 && exercise s1 == exercise s2
      && similarity (exercise s1) (stateContext s1) (stateContext s2)

onefirst :: State a -> Either String (StepInfo a, State a)
onefirst state =
   case allfirsts state of
      Right []     -> Left "No step possible"
      Right (hd:_) -> Right hd
      Left msg     -> Left msg

onefinal :: State a -> Either String (Context a)
onefinal = fmap lastTerm . solution Nothing

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
   g c = [ (r, location new, state { statePrefix = noPrefix, stateContext = new })
         | r   <- ruleset ex
         , (r, location c) `notElem` ps
         , new <- applyAll r c
         ]

   cmp (r1, loc1, _) (r2, loc2, _) =
      case ruleOrdering ex r1 r2 of
         EQ   -> loc1 `compare` loc2
         this -> this

stepsremaining :: State a -> Either String Int
stepsremaining = mapSecond derivationLength . solution Nothing

findbuggyrules :: State a -> Context a -> [(Rule (Context a), Location, Environment)]
findbuggyrules state a =
   [ (r, loc, as)
   | r         <- filter isBuggy (ruleset ex)
   , (loc, as) <- recognizeRule ex r (stateContext state) a
   ]
 where
   ex = exercise state

-- Recognize a rule at (possibly multiple) locations
recognizeRule :: Exercise a -> Rule (Context a) -> Context a -> Context a -> [(Location, Environment)]
recognizeRule ex r ca cb = rec (top ca)
 where
   final = addTransRecognizer (similarity ex) r
   rec x = do
      -- here
      as <- recognizeAll final x cb
      return (location x, as)
    `mplus` -- or there
      concatMap rec (downs x)

exampleDerivations :: Exercise a -> Either String [Derivation (StepInfo a) (Context a)]
exampleDerivations ex = mapM (solution Nothing . emptyState ex) (examplesAsList ex)