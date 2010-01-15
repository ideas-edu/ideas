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
-- Diagnose a term submitted by a student. Deprecated (see diagnose service)
--
-----------------------------------------------------------------------------
module Service.Submit where

import qualified Common.Apply as Apply
import Common.Exercise
import Common.Transformation
import Common.Context
import Common.Utils (safeHead)
import Service.TypedAbstractService
import Data.Maybe

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]   
              | NotEquivalent      
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent
              
-- To be removed, and replaced by Diagnose service
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
   expected = do
      xs <- allfirsts state
      let p (_, _, ns) = similarity (exercise state) new (term ns)
      safeHead (filter p xs)

   discovered searchForBuggy = safeHead
      [ r
      | r <- ruleset (exercise state)
      , isBuggyRule r == searchForBuggy
      , a <- Apply.applyAll r (inContext sub1)
      , similarity (exercise state) sub2 (fromContext a)
      ]
    where 
      mode = not searchForBuggy
      diff = difference (exercise state) mode (term state) new
      (sub1, sub2) = fromMaybe (term state, new) diff      

getResultState :: Result a -> Maybe (State a)
getResultState result =
   case result of
      Ok _ st     -> return st
      Detour _ st -> return st
      Unknown st  -> return st
      _           -> Nothing