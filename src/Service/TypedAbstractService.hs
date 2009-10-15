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
   ( State(..), emptyState, term
   , stepsremaining, findbuggyrules, submit, ready, allfirsts
   , derivation, onefirst, applicable, apply, generate
   , Result(..), getResultState, resetStateIfNeeded
   ) where

import qualified Common.Apply as Apply
import Common.Context 
import Common.Exercise (Exercise(..), ruleset, randomTermWith)
import Common.Strategy (Prefix, emptyPrefix, prefixToSteps, stepsToRules, runPrefixMajor, lastRuleInPrefix)
import Common.Transformation (Rule, name, isMajorRule, isBuggyRule)
import Common.Utils (safeHead)
import Data.Maybe
import System.Random

data State a = State 
   { exercise :: Exercise a
   , prefix   :: Maybe (Prefix (Context a))
   , context  :: Context a
   }

term :: State a -> a
term = fromContext . context

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]   
              | NotEquivalent      
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent

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

{- Old implementation: to be removed
derivation :: State a -> [(Rule (Context a), Context a)]
derivation state = fromMaybe (error "derivation") $ do
   p0 <- prefix state
   (final, p1) <- safeHead (runPrefix p0 (context state))
   let steps = drop (length (prefixToSteps p0)) (prefixToSteps p1)
       rules = stepsToRules steps
       terms = let run x []     = [ [] | similarity (exercise state) (fromContext x) (fromContext final) ]
                   run x (r:rs) = [ y:ys | y <- Apply.applyAll r x, ys <- run y rs ] 
               in fromMaybe [] $ safeHead (run (context state) rules)
       check = isMajorRule . fst
   return $ filter check $ zip rules terms -}
   
derivation :: State a -> [(Rule (Context a), Context a)]
derivation state =
   case allfirsts state of 
      [] -> []
      (r, _, next):_ -> (r, context next) : derivation next

-- The last condition in the list comprehension is to avoid a very subtle case in which some steps
-- remain to be done (in the prefix), but those steps are administrative (not even minor rules, but 
-- markers for the beginning and the end of a sub-strategy). This is a quick fix. To do: inspect other
-- locations where runPrefixUntil is called.
allfirsts :: State a -> [(Rule (Context a), Location, State a)]
allfirsts state = fromMaybe (error "allfirsts") $ do
   p0 <- prefix state
   let f (a, p1) = 
          [ (r, location a, state {context = a, prefix = Just p1})
          | Just r <- [lastRuleInPrefix p1], isMajorRule r, stepsToRules (prefixToSteps p0) /= stepsToRules (prefixToSteps p1)
          ]
   return $ concatMap f $ runPrefixMajor p0 $ context state

onefirst :: State a -> (Rule (Context a), Location, State a)
onefirst = fromMaybe (error "onefirst") . safeHead . allfirsts

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc state =
   let check r = not (isBuggyRule r) && Apply.applicable r (setLocation loc (context state))
   in filter check (ruleset (exercise state))

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the 
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Rule (Context a) -> Location -> State a -> State a
apply r loc state = maybe applyOff applyOn (prefix state)
 where
   applyOn _ = -- scenario 1: on-strategy
      fromMaybe applyOff $ safeHead
      [ s1 | (r1, loc1, s1) <- allfirsts state, name r == name r1, loc==loc1 ]
      
   applyOff  = -- scenario 2: off-strategy
      case Apply.apply r (setLocation loc (context state)) of
         Just new -> state { context=new }
         Nothing  -> error "apply"
       
ready :: State a -> Bool
ready state = isReady (exercise state) (term state)

stepsremaining :: State a -> Int
stepsremaining = length . derivation

findbuggyrules :: State a -> Context a -> [Rule (Context a)]
findbuggyrules state a =
   let ex      = exercise state
       isA     = similarity ex (fromContext a) . fromContext  
       buggies = filter isBuggyRule (ruleset ex)
       check r = any isA (Apply.applyAll r (context state))
   in filter check buggies

-- make sure that new has a prefix (because of possible detour)
-- when resetting the prefix, also make sure that the context is refreshed
resetStateIfNeeded :: State a -> State a
resetStateIfNeeded s 
   | isJust (prefix s) = s
   | otherwise = s
        { prefix  = Just (emptyPrefix (strategy (exercise s)))
        , context = inContext (fromContext (context s))
        } 

submit :: State a -> a -> Result a
submit state new
   -- Is the submitted term equivalent?
   | not (equivalence (exercise state) (term state) new) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just r -> -- report the buggy rule
              Buggy [r]
           Nothing -> -- unknown mistake
              NotEquivalent
   -- Is the submitted term (very) similar to the previous one? 
   | similarity (exercise state) (term state) new =
        -- If yes, report this
        Ok [] state
   -- Was the submitted term expected by the strategy
   | isJust expected =
        -- If yes, return new state and rule
        let (r, _, ns) = fromJust expected  
        in Ok [r] ns
   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        case discovered False of
           Just r ->  -- If yes, report the found rule as a detour
              Detour [r] state { prefix=Nothing, context=inContext new }
           Nothing -> -- If not, we give up
              Unknown state { prefix=Nothing, context=inContext new }
 where
   expected = 
      let p (_, _, ns) = similarity (exercise state) new (term ns)
      in safeHead (filter p (allfirsts state))
 
   (sub1, sub2) = fromMaybe (term state, new) $
      newDifference (exercise state) True (term state) new
 
   discovered useBuggyRules = safeHead
      [ r
      | r <- ruleset (exercise state)
      , isBuggyRule r == useBuggyRules
      , a <- Apply.applyAll r (inContext sub1)
      , similarity (exercise state) sub2 (fromContext a)
      ]

getResultState :: Result a -> Maybe (State a)
getResultState result =
   case result of
      Ok _ st     -> return st
      Detour _ st -> return st
      Unknown st  -> return st
      _           -> Nothing