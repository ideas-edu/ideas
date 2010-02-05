-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.TypedAbstractService 
   ( -- * Exercise state
     State(..), emptyState, term
     -- * Services
   , stepsremaining, findbuggyrules, ready, allfirsts, derivation
   , onefirst, applicable, apply, generate, generateWith
   ) where

import qualified Common.Apply as Apply
import Common.Context 
import Common.Derivation hiding (derivation)
import Common.Exercise (Exercise(..), ruleset, randomTermWith)
import Common.Strategy hiding (not, fail)
import Common.Transformation (Rule, name, isMajorRule, isBuggyRule)
import Common.Utils (safeHead)
import Data.Maybe
import System.Random
import Control.Monad

data State a = State 
   { exercise     :: Exercise a
   , prefix       :: Maybe (Prefix (Context a))
   , context      :: Context a
   }

term :: State a -> a
term = fromJust . fromContext . context

-----------------------------------------------------------

emptyState :: Exercise a -> a -> State a
emptyState ex a = State
   { exercise = ex
   , prefix   = Just (emptyPrefix (strategy ex))
   , context  = inContext a
   }
      
-- result must be in the IO monad to access a standard random number generator
generate :: Exercise a -> Int -> IO (State a)
generate ex level = do 
   stdgen <- newStdGen
   return (generateWith stdgen ex level)

generateWith :: StdGen -> Exercise a -> Int -> State a
generateWith rng ex level = emptyState ex (randomTermWith rng level ex)

derivation :: Monad m => Maybe StrategyConfiguration -> State a -> m [(Rule (Context a), Context a)]
derivation mcfg state =
   case (prefix state, mcfg) of 
      (Nothing, _) -> fail "Prefix is required"
      -- configuration is only allowed beforehand: hence, the prefix 
      -- should be empty (or else, the configuration is ignored). This
      -- restriction should probably be relaxed later on.
      (Just p, Just cfg) | null (prefixToSteps p) -> 
         let new = configure cfg $ strategy $ exercise state
         in rec state 
               { prefix   = Just (emptyPrefix new)
               , exercise = (exercise state) {strategy=new}
               } 
      _ -> rec state
 where
   rec :: Monad m => State a -> m [(Rule (Context a), Context a)]
   rec state = do
      xs <- allfirsts state
      case xs of 
         [] -> return []
         (r, _, next):_ -> liftM ((r, context next):) (rec next)

-- Note that we have to inspect the last step of the prefix afterwards, because
-- the remaining part of the derivation could consist of minor rules only.
allfirsts :: Monad m => State a -> m [(Rule (Context a), Location, State a)]
allfirsts state = 
   case prefix state of
      Nothing -> 
         fail "Prefix is required"
      Just p0 ->
         let tree = cutOnStep (stop . lastStepInPrefix) (prefixTree p0 (context state))
         in return (mapMaybe make (derivations tree))
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
onefirst state = 
   case allfirsts state of
      Right (hd:_) -> return hd
      Right []     -> fail "No step possible"
      Left msg     -> fail msg

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc state =
   let check r = not (isBuggyRule r) && Apply.applicable r (setLocation loc (context state))
   in filter check (ruleset (exercise state))

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
ready state = isReady (exercise state) (term state)

stepsremaining :: Monad m => State a -> m Int
stepsremaining = liftM length . derivation Nothing

findbuggyrules :: State a -> a -> [Rule (Context a)]
findbuggyrules state a =
   let ex      = exercise state
       isA     = maybe False (similarity ex a) . fromContext
       buggies = filter isBuggyRule (ruleset ex)
       check r = any isA (Apply.applyAll r (context state))
   in filter check buggies