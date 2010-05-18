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
   ( Diagnosis(..), RuleID, diagnose, restartIfNeeded
   , diagnosisType, diagnosisTypeSynonym
   ) where 

import Common.Context
import Common.Exercise
import Common.Strategy (emptyPrefix)
import Common.Transformation
import Common.Utils (safeHead)
import Data.Maybe
import Service.Types
import Service.TypedAbstractService
import qualified Service.Types as Tp

----------------------------------------------------------------
-- Result types for diagnose service

type RuleID a = Rule (Context a)

data Diagnosis a
   = Buggy          (RuleID a)
   | NotEquivalent  
   | Similar        Bool (State a)
   | Expected       Bool (State a) (RuleID a)
   | Detour         Bool (State a) (RuleID a)
   | Correct        Bool (State a)

----------------------------------------------------------------
-- The diagnose service

diagnose :: State a -> a -> Diagnosis a
diagnose state new
   -- Is the submitted term equivalent?
   | not (equivalence ex (term state) new) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just r -> -- report the buggy rule
              Buggy r
           Nothing -> -- unknown mistake
              NotEquivalent
              
   -- Is the submitted term (very) similar to the previous one? 
   | similarity ex (term state) new =
        -- If yes, report this
        Similar (ready state) state
        
   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let (r, _, ns) = fromJust expected  
        in Expected (ready ns) ns r

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        let ns = restartIfNeeded (state { prefix=Nothing, context=inContext ex new })
        in case discovered False of
              Just r ->  -- If yes, report the found rule as a detour
                 Detour (ready ns) ns r
              Nothing -> -- If not, we give up
                 Correct (ready ns) ns
 where
   ex = exercise state
   
   expected = do
      xs <- allfirsts (restartIfNeeded state)
      let p (_, _, ns) = similarity ex new (term ns)
      safeHead (filter p xs)

   discovered searchForBuggy = safeHead
      [ r
      | r <- ruleset ex
      , isBuggyRule r == searchForBuggy
      , recognizeRule ex r sub1 sub2
      ]
    where 
      (sub1, sub2) = 
         case difference ex (not searchForBuggy) (term state) new of 
            Just (a, b) -> (inContext ex a, inContext ex b) 
            Nothing     -> (context state, inContext ex new)
 
----------------------------------------------------------------
-- Helpers

-- If possible (and if needed), restart the strategy
-- Make sure that the new state has a prefix
-- When resetting the prefix, also make sure that the context is refreshed
restartIfNeeded :: State a -> State a
restartIfNeeded s 
   | isNothing (prefix s) && canBeRestarted ex = 
        case fromContext (context s) of 
           Just a -> s
              { prefix  = Just (emptyPrefix (strategy ex))
              , context = inContext ex a
              } 
           Nothing -> s
   | otherwise = s
 where
   ex = exercise s
   
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