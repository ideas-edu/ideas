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
import Common.Context  (Location, Context, inContext, location, currentFocus, Uniplate, setLocation)
import Common.Exercise (Exercise(..))
import Common.Strategy (Prefix, emptyPrefix, runPrefix, prefixToSteps, stepsToRules, runPrefixMajor, lastRuleInPrefix)
import Common.Transformation (Rule, name, isMajorRule, isBuggyRule)
import Common.Utils (safeHead)
import Data.Maybe
import System.Random
import Debug.Trace
import qualified Test.QuickCheck as QC

type State a = (Exercise (Context a), Maybe (Prefix (Context a)), Context a)

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]   
              | NotEquivalent      
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent
            
-- result must be in the IO monad to access a standard random number generator
generate :: Exercise (Context a) -> Int -> IO (State a)
generate ex level = do 
   stdgen <- newStdGen
   case QC.generate 100 stdgen (generator ex) of
      a | suitableTerm ex a ->
             return (ex, Just (emptyPrefix (strategy ex)), a)
      _   -> generate ex level 

derivation :: State a -> [(Rule (Context a), Context a)]
derivation (ex, mp, ca) = fromMaybe (error "derivation") $ do
   p0 <- mp
   (final, p1) <- maybe (error $ "!!!" ++ show p0 ++ prettyPrinter ex ca ++ show (map snd $ runPrefix p0 ca)) Just $ safeHead (runPrefix p0 ca)
   let steps = drop (length (prefixToSteps p0)) (prefixToSteps p1)
       rules = stepsToRules steps
       terms = let run x []     = [ [] | equality ex x final ]
                   run x (r:rs) = [ y:ys | y <- Apply.applyAll r x, ys <- run y rs ] 
               in fromMaybe [] $ safeHead (run ca rules)
       check = isMajorRule . fst
   return $ filter check $ zip rules terms

allfirsts :: State a -> [(Rule (Context a), Location, State a)]
allfirsts (ex, mp, ca) = fromMaybe (error "allfirsts") $ do
   p0 <- mp
   let f (a, p1) = 
          [ (r, location a, (ex, Just p1, a))
          | Just r <- [lastRuleInPrefix p1], isMajorRule r
          ]
   return $ concatMap f $ runPrefixMajor p0 ca

onefirst :: State a -> (Rule (Context a), Location, State a)
onefirst = fromMaybe (error "onefirst") . safeHead . allfirsts

applicable :: Location -> State a -> [Rule (Context a)]
applicable loc (ex, _, ca) = filter (`Apply.applicable` (setLocation loc ca)) (ruleset ex)

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the 
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Rule (Context a) -> Location -> State a -> State a
apply r loc s@(ex, mp, ca) = maybe applyOff applyOn mp
 where
   applyOn p = -- scenario 1: on-strategy
      fromMaybe applyOff $ safeHead
      [ s1 | (r1, loc1, s1) <- allfirsts s, name r == name r1, loc==loc1 ]
      
   applyOff  = -- scenario 2: off-strategy
      case Apply.apply r (setLocation loc ca) of
         Just new -> (ex, mp, new)
         Nothing  -> error "apply"
            
ready :: State a -> Bool
ready (ex, _, ca) = finalProperty ex ca

stepsremaining :: State a -> Int
stepsremaining = length . derivation

-- For now, only one rule look-ahead (for buggy rules and for sound rules)
submit :: State a -> Context a -> Result a
submit s@(ex, mp, old) new
   | not (equivalence ex old new) =
        case safeHead (filter isBuggyRule (findRules ex old new)) of
           Just br -> Buggy [br]
           Nothing -> NotEquivalent
   | equality ex old new =
        Ok [] s
   | otherwise =
        maybe applyOff applyOn mp

 where
   applyOn p = -- scenario 1: on-strategy
      fromMaybe applyOff $ safeHead
      [ Ok [r1] s1 | (r1, loc1, s1@(_, _, s1a)) <- allfirsts s, equality ex new s1a ]      
   
   applyOff = -- scenario 2: off-strategy
      let newState = (ex, Nothing, new)
      in case safeHead (filter (not . isBuggyRule) (findRules ex old new)) of
              Just r  -> Detour [r] newState
              Nothing -> Unknown newState
              
-- local helper-function
findRules :: Exercise a -> a -> a -> [Rule a]
findRules ex old new = 
   filter (maybe False (equality ex new) . (`Apply.apply` old)) (ruleset ex)