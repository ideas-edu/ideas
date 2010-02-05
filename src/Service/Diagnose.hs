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
-- Diagnose a term submitted by a student
--
-----------------------------------------------------------------------------
module Service.Diagnose 
   ( Diagnosis(..), RuleID, diagnose, restartIfNeeded
   ) where 

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Strategy (emptyPrefix)
import Common.Transformation
import Common.Utils (safeHead)
import Data.Maybe
import Service.TypedAbstractService

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
      , ca <- applyAll r (inContext ex sub1)
      -- , let s = prettyPrinter (exercise state) (fromContext a)
      --, if s=="2*x+2 == 5" then True else error s
      , a <- fromContext ca
      , similarity ex sub2 a
      ]
    where 
      mode = not searchForBuggy
      diff = difference ex mode (term state) new
      (sub1, sub2) = fromMaybe (term state, new) diff
      
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