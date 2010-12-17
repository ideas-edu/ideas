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
-- Diagnose a term submitted by a student
--
-----------------------------------------------------------------------------
module Service.Diagnose 
   ( Diagnosis(..), diagnose, restartIfNeeded
   , diagnosisType, diagnosisTypeSynonym
   ) where 

import Common.Library
import Common.Utils (safeHead)
import Data.List (sortBy)
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.BasicServices
import Service.Types

----------------------------------------------------------------
-- Result types for diagnose service

data Diagnosis a
   = Buggy          (Rule (Context a))
   | NotEquivalent  
   | Similar        Bool (State a)
   | Expected       Bool (State a) (Rule (Context a))
   | Detour         Bool (State a) (Rule (Context a))
   | Correct        Bool (State a)

instance Show (Diagnosis a) where
   show diagnosis = 
      case diagnosis of
         Buggy r        -> "Buggy rule " ++ show (show r)
         NotEquivalent  -> "Unknown mistake" 
         Similar _ _    -> "Very similar"
         Expected _ _ r -> "Rule " ++ show (show r) ++ ", expected by strategy"
         Detour _ _ r   -> "Rule " ++ show (show r) ++ ", not following strategy"
         Correct _ _    -> "Unknown step"

----------------------------------------------------------------
-- The diagnose service

diagnose :: State a -> a -> Diagnosis a
diagnose state new
   -- Is the submitted term equivalent?
   | not (equivalenceContext ex (stateContext state) newc) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just r -> -- report the buggy rule
              Buggy r
           Nothing -> -- unknown mistake
              NotEquivalent
              
   -- Is the submitted term (very) similar to the previous one? 
   | similarity ex (stateTerm state) new =
        -- If yes, report this
        Similar (ready state) state
        
   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let (r, _, ns) = fromJust expected  
        in Expected (ready ns) ns r

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        let ns = restartIfNeeded (makeState pkg Nothing newc)
        in case discovered False of
              Just r ->  -- If yes, report the found rule as a detour
                 Detour (ready ns) ns r
              Nothing -> -- If not, we give up
                 Correct (ready ns) ns
 where
   pkg  = exercisePkg state
   ex   = exercise pkg
   newc = inContext ex new
   
   expected = do
      xs <- allfirsts (restartIfNeeded state)
      let p (_, _, ns) = similarity ex new (stateTerm ns)
      safeHead (filter p xs)

   discovered searchForBuggy = safeHead
      [ r
      | r <- sortBy (ruleOrdering ex) (ruleset ex)
      , isBuggyRule r == searchForBuggy
      , ruleIsRecognized ex r sub1 sub2
      ]
    where 
      (sub1, sub2) = 
         case difference ex (not searchForBuggy) (stateTerm state) new of 
            Just (a, b) -> (inContext ex a, inContext ex b) 
            Nothing     -> (stateContext state, newc)
 
----------------------------------------------------------------
-- Helpers

-- If possible (and if needed), restart the strategy
-- Make sure that the new state has a prefix
-- When resetting the prefix, also make sure that the context is refreshed
restartIfNeeded :: State a -> State a
restartIfNeeded state 
   | isNothing (statePrefix state) && canBeRestarted (exercise pkg) = 
        emptyState pkg (stateTerm state)
   | otherwise = state
 where
   pkg = exercisePkg state
   
diagnosisType :: Type a (Diagnosis a)
diagnosisType = useSynonym diagnosisTypeSynonym

diagnosisTypeSynonym :: TypeSynonym a (Diagnosis a)
diagnosisTypeSynonym = typeSynonym "Diagnosis" to from tp
 where
   to (Left r) = Buggy r
   to (Right (Left ())) = NotEquivalent
   to (Right (Right (Left (b, s)))) = Similar b s
   to (Right (Right (Right (Left (b, s, r))))) = Expected b s r
   to (Right (Right (Right (Right (Left (b, s, r)))))) = Detour b s r
   to (Right (Right (Right (Right (Right (b, s)))))) = Correct b s
   
   from (Buggy r)        = Left r
   from (NotEquivalent)  = Right (Left ())
   from (Similar b s)    = Right (Right (Left (b, s)))
   from (Expected b s r) = Right (Right (Right (Left (b, s, r))))
   from (Detour b s r)   = Right (Right (Right (Right (Left (b, s, r)))))
   from (Correct b s)    = Right (Right (Right (Right (Right (b, s)))))
   
   tp  =  Rule
      :|: Unit
      :|: Pair   Bool stateTp
      :|: tuple3 Bool stateTp Rule
      :|: tuple3 Bool stateTp Rule
      :|: Pair   Bool stateTp