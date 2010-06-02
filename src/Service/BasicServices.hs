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
module Service.BasicServices 
   ( -- * Basic Services
     stepsremaining, findbuggyrules, ready, allfirsts, derivation
   , onefirst, applicable, apply, generate, generateWith
   ) where

import Common.Context 
import Common.Derivation hiding (derivation)
import Common.Exercise   hiding (generate)
import Common.Strategy   hiding (not, fail)
import Common.Transformation (Rule, name, isMajorRule, isBuggyRule)
import Common.Utils (safeHead)
import Common.Navigator
import Data.List
import Data.Maybe
import System.Random
import Control.Monad
import Service.ExercisePackage
import Service.State
import qualified Common.Apply as Apply
      
-- result must be in the IO monad to access a standard random number generator
generate :: ExercisePackage a -> Int -> IO (State a)
generate pkg level = do 
   stdgen <- newStdGen
   return (generateWith stdgen pkg level)

generateWith :: StdGen -> ExercisePackage a -> Int -> State a
generateWith rng pkg level = 
   emptyState pkg (randomTermWith rng level (exercise pkg))

derivation :: Monad m => Maybe StrategyConfiguration -> State a -> m [(Rule (Context a), Context a)]
derivation mcfg state =
   case (prefix state, mcfg) of 
      (Nothing, _) -> fail "Prefix is required"
      -- configuration is only allowed beforehand: hence, the prefix 
      -- should be empty (or else, the configuration is ignored). This
      -- restriction should probably be relaxed later on.
      (Just p, Just cfg) | null (prefixToSteps p) -> 
         let oldStrategy   = strategy ex
             newStrategy   = configure cfg oldStrategy
             updatePkg pkg = pkg {exercise = updateEx (exercise pkg)}
             updateEx  ex  = ex  {strategy = newStrategy} 
         in rec timeout [] state 
               { prefix      = Just (emptyPrefix newStrategy)
               , exercisePkg = updatePkg (exercisePkg state)
               } 
      _ -> rec timeout [] state
 where
   ex = exercise (exercisePkg state)
   timeout = 50
 
   rec i acc state = 
      case onefirst state of
         Nothing           -> return (reverse acc)
         Just (r, _, next)
            |  i <= 0      -> fail msg
            | otherwise    -> rec (i-1) ((r, context next) : acc) next
    where
      msg = "Time out after " ++ show timeout ++ " steps. " ++ 
            concatMap f (reverse acc)
      f (r, c) = let s = maybe "???" (prettyPrinter ex) (fromContext c)
                 in "[" ++ show r ++ "]  " ++ s ++ "; "

-- Note that we have to inspect the last step of the prefix afterwards, because
-- the remaining part of the derivation could consist of minor rules only.
allfirsts :: Monad m => State a -> m [(Rule (Context a), Location, State a)]
allfirsts state = 
   case prefix state of
      Nothing -> 
         fail "Prefix is required"
      Just p0 ->
         let tree = cutOnStep (stop . lastStepInPrefix) (prefixTree p0 (context state))
             f (r1, _, _) (r2, _, _) = 
                ruleOrdering (exercise (exercisePkg state)) r1 r2
         in return (sortBy f (mapMaybe make (derivations tree)))
 where
   stop (Just (Step r)) = isMajorRule r
   stop _ = False
   
   make d = do
      prefixEnd <- safeHead (reverse (steps d))
      termEnd   <- safeHead (reverse (terms d))
      case lastStepInPrefix prefixEnd of
         Just (Step r) | isMajorRule r -> return
            ( r
            , location termEnd
            , state { context = termEnd
                    , prefix  = Just prefixEnd
                    }
            )
         _ -> Nothing

onefirst :: Monad m => State a -> m (Rule (Context a), Location, State a)
onefirst state = do
   xs <- allfirsts state
   case xs of
      hd:_ -> return hd
      []   -> fail "No step possible"

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc state =
   let check r = not (isBuggyRule r) && Apply.applicable r (setLocation loc (context state))
   in filter check (ruleset (exercise (exercisePkg state)))

-- local helper
setLocation :: Location -> Context a -> Context a 
setLocation loc c0 = fromMaybe c0 $ do
   navigateTo loc c0

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the 
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Monad m => Rule (Context a) -> Location -> State a -> m (State a)
apply r loc state = maybe applyOff applyOn (prefix state)
 where
   applyOn _ = -- scenario 1: on-strategy
      maybe applyOff return $ safeHead
      [ s1 | (r1, loc1, s1) <- fromMaybe [] $ allfirsts state, name r == name r1, loc==loc1 ]
      
   applyOff  = -- scenario 2: off-strategy
      case Apply.apply r (setLocation loc (context state)) of
         Just new -> return state { context=new, prefix=Nothing }
         Nothing  -> fail ("Cannot apply " ++ show r)
       
ready :: State a -> Bool
ready state = isReady (exercise (exercisePkg state)) (term state)

stepsremaining :: Monad m => State a -> m Int
stepsremaining = liftM length . derivation Nothing

findbuggyrules :: State a -> a -> [Rule (Context a)]
findbuggyrules state a =
   let ex      = exercise (exercisePkg state)
       buggies = filter isBuggyRule (ruleset ex)
       check r = ruleIsRecognized ex r (context state) (inContext ex a)
   in filter check buggies