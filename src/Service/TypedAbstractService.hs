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
module Service.TypedAbstractService where

import qualified Common.Apply as Apply
import Common.Context 
import Common.Exercise (Exercise(..))
import Common.Strategy (Prefix, emptyPrefix, runPrefix, prefixToSteps, stepsToRules, runPrefixMajor, lastRuleInPrefix)
import Common.Transformation (Rule, name, isMajorRule, isBuggyRule)
import Common.Utils (safeHead)
import Domain.Logic.FeedbackText -- FIXME
import Service.SearchSpace (searchSpace)
import Service.Progress
import Data.Maybe
import System.Random
import qualified Test.QuickCheck as QC

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

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
-- !! Placed here to avoid a cyclic import dependency. Should be moved 
-- to Domain.Logic in future
feedbackLogic :: Result a -> (String, Bool)
feedbackLogic result =
   case result of
      Buggy rs        -> (feedbackBuggy rs, False)
      NotEquivalent   -> (feedbackNotEquivalent, False)
      Ok rs _
         | null rs    -> (feedbackSame, False)
         | otherwise  -> feedbackOk rs
      Detour rs _     -> feedbackDetour rs
      Unknown _       -> (feedbackUnknown, False)

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
   case QC.generate 100 stdgen (generator ex) of
      a | suitableTerm ex a -> return (emptyState ex a)
      _ -> generate ex level

derivation :: State a -> [(Rule (Context a), Context a)]
derivation state = fromMaybe (error "derivation") $ do
   p0 <- prefix state
   (final, p1) <- safeHead (runPrefix p0 (context state))
   let steps = drop (length (prefixToSteps p0)) (prefixToSteps p1)
       rules = stepsToRules steps
       terms = let run x []     = [ [] | equality (exercise state) (fromContext x) (fromContext final) ]
                   run x (r:rs) = [ y:ys | y <- Apply.applyAll r x, ys <- run y rs ] 
               in fromMaybe [] $ safeHead (run (context state) rules)
       check = isMajorRule . fst
   return $ filter check $ zip rules terms

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

-- FIXME: specialized output for logic solver
onefirsttext :: State a -> (Bool, String, State a)
onefirsttext state =
   case allfirsts state of
      (r, _, s):_ -> (True, "Use " ++ fromMaybe ("rule " ++ name r) (ruleText r), s)
      _ -> (False, "Sorry, no hint available", state)

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
ready state = finalProperty (exercise state) (term state)

stepsremaining :: State a -> Int
stepsremaining = length . derivation

-- FIXME: specialized output for logic solver
submittext :: State a -> a -> (Bool, String, State a)
submittext state a = 
   case getResultState result of
      Just new | b -> (True, txt, resetStateIfNeeded new)
      _ -> (False, txt, state)
 where
  result = submit state a
  (txt, b) = feedbackLogic result

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
submit state = fst . submitExtra state

submitExtra :: State a -> a -> (Result a, Int)
submitExtra state new
   | not (equivalence (exercise state) (term state) new) =
        {-
        case safeHead (filter isBuggyRule (findRules (exercise state) (context state) new)) of  
           Just br -> Buggy [br]  
           Nothing -> NotEquivalent  -}
        
        case filter isSame $ successesAfter $ maxNumber 200 {- maxDepth 3 -} errSpace of
           ((_, _, rs), n):_ -> (Buggy rs, n)
           _                 -> (NotEquivalent, 200)
   | equality (exercise state) (term state) new =
        (Ok [] state, 0)
   | otherwise =
        {-
        case fmap applyOn (prefix state) of
           Just (Just r) -> (r, 0)
           _ -> (Unknown state { context=inContext new }, 200) -}
        
        case filter isSame $ successesAfter $ maxNumber 200 $ {- maxDepth 5 $ -} space of
           ((a, mp, rs), n):_ 
              | isJust mp -> (Ok rs state { context=a, prefix=mp }, n)
              | otherwise -> (Detour rs state { context=a, prefix=mp }, n)
           _ -> (Unknown state { context=inContext new }, 200)
 where 
   isSame ((a, _, _), _) = equality (exercise state) new (fromContext a)
 
   space    = searchSpace (getOrdering state) diff (prefix state) (filter (not . isBuggyRule) $ ruleset $ exercise state) (context state)
   errSpace = searchSpace (getOrdering state) diff Nothing (ruleset $ exercise state) (context state)
   
   diff a = map (f a) $ differences (exercise state) new (fromContext a)
   f a (is, td) = (setLocation (makeLocation is) a, td)
   
{-
   applyOn _ = -- scenario 1: on-strategy
      safeHead
      [ Ok [r1] s1 | (r1, _, s1) <- allfirsts state, equality (exercise state) new (term s1) ]      -}
{-
   applyOff = -- scenario 2: off-strategy
      let newState = state { context=inContext new } 
          -- Prefix a -> [(score, Rule a)] -> a -> Progress score (a, [Rule a])
      in case safeHead (filter (not . isBuggyRule) (findRules (exercise state) (context state) new)) of
            Just r  -> Detour [r] newState
            Nothing -> Unknown newState -}

getOrdering :: State a -> Context a -> Context a -> Ordering
getOrdering state a b = ordering (exercise state) (fromContext a) (fromContext b)
{-
-- local helper-function
findRules :: Exercise a -> Context a -> a -> [Rule (Context a)]
findRules ex old new = filter p (ruleset ex)
 where
   p r = any (equality ex new . fromContext) (everywhere ex (Apply.applyAll r) old)-}
   
getResultState :: Result a -> Maybe (State a)
getResultState result =
   case result of
      Ok _ st     -> return st
      Detour _ st -> return st
      Unknown st  -> return st
      _           -> Nothing