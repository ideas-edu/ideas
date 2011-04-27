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
   , newState
   , diagnosisType
   ) where 

import Common.Library
import Common.Utils (safeHead)
import Data.List (sortBy)
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.BasicServices hiding (apply)
import Service.Types

----------------------------------------------------------------
-- Result types for diagnose service

data Diagnosis a
   = Buggy          (Rule (Context a))
--   | Missing
--   | IncorrectPart  [a]
   | NotEquivalent  
   | Similar        Bool (State a)
   | Expected       Bool (State a) (Rule (Context a))
   | Detour         Bool (State a) (Rule (Context a))
   | Correct        Bool (State a)

instance Show (Diagnosis a) where
   show diagnosis = 
      case diagnosis of
         Buggy r          -> "Buggy rule " ++ show (show r)
--         Missing          -> "Missing solutions"
--         IncorrectPart xs -> "Incorrect parts (" ++ show (length xs) ++ " items)"
         NotEquivalent    -> "Unknown mistake" 
         Similar _ _      -> "Very similar"
         Expected _ _ r   -> "Rule " ++ show (show r) ++ ", expected by strategy"
         Detour _ _ r     -> "Rule " ++ show (show r) ++ ", not following strategy"
         Correct _ _      -> "Unknown step"

newState :: Diagnosis a -> Maybe (State a)
newState diagnosis = 
   case diagnosis of
      Buggy _        -> Nothing
      NotEquivalent  -> Nothing
      Similar  _ s   -> Just s
      Expected _ s _ -> Just s
      Detour   _ s _ -> Just s
      Correct  _ s   -> Just s

----------------------------------------------------------------
-- The diagnose service

diagnose :: State a -> a -> Diagnosis a
diagnose state new
   -- Is the submitted term equivalent?
   | not (equivalence ex (stateContext state) newc) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True of
           Just r  -> Buggy r -- report the buggy rule
           Nothing -> NotEquivalent -- compareParts state new
              
   -- Is the submitted term (very) similar to the previous one? 
   | similarity ex (stateContext state) newc =
        -- If yes, report this
        Similar (ready state) state
        
   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let (r, _, _, ns) = fromJust expected  
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
      let xs = either (const []) id $ allfirsts (restartIfNeeded state)
          p (_, _, _, ns) = similarity ex newc (stateContext ns)
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
diagnosisType = Iso f g tp
 where
   f (Left (Left r)) = Buggy r
--   f (Left (Right (Left ()))) = Missing
--   f (Left (Right (Right (Left xs)))) = IncorrectPart xs
   f (Left (Right ())) = NotEquivalent
   f (Right (Left (b, s))) = Similar b s
   f (Right (Right (Left (b, s, r)))) = Expected b s r
   f (Right (Right (Right (Left (b, s, r))))) = Detour b s r
   f (Right (Right (Right (Right (b, s))))) = Correct b s

   g (Buggy r)          = Left (Left r)
--   g Missing            = Left (Right (Left ()))
--   g (IncorrectPart xs) = Left (Right (Right (Left xs)))
   g NotEquivalent      = Left (Right ())
   g (Similar b s)      = Right (Left (b, s))
   g (Expected b s r)   = Right (Right (Left (b, s, r)))
   g (Detour b s r)     = Right (Right (Right (Left (b, s, r))))
   g (Correct b s)      = Right (Right (Right (Right (b, s))))
   
   tp  =  
       (  Tag "buggy"         Rule
--      :|: Tag "missing"       Unit
--      :|: Tag "incorrectpart" (List Term)
      :|: Tag "notequiv"      Unit
       )
      :|: 
       (  Tag "similar"  (Pair   readyBool stateType)
      :|: Tag "expected" (tuple3 readyBool stateType Rule)
      :|: Tag "detour"   (tuple3 readyBool stateType Rule)
      :|: Tag "correct"  (Pair   readyBool stateType)
       )
      
   readyBool = Tag "ready" Bool

----------------------------------------------------------------
-- Compare answer sets (and search for missing parts/incorrect parts)
{-
compareParts :: State a -> a -> Diagnosis a
compareParts state = answerList eq split solve (stateTerm state)
 where
   ex    = exercise (exercisePkg state)
   eq    = equivalence ex
   split = splitParts ex
   solve = \a -> fromMaybe a $ 
                    apply (strategy ex) (inContext ex a) >>= fromContext
   
answerList :: (a -> a -> Bool) -> (a -> [a]) -> (a -> a) -> a -> a -> Diagnosis a
answerList eq split solve a b
   | noSplit               = NotEquivalent
   | present && null wrong = NotEquivalent -- error situation
   | null wrong            = Missing
   | partly                = IncorrectPart wrong
   | otherwise             = NotEquivalent
 where
   as = split (solve a) -- expected
   ps = [ (x, split (solve x)) | x <- split b ] -- student (keep original parts)
   bs = concatMap snd ps -- student answer, but then fully solved
   wrong   = [ x | (x, xs) <- ps, any notInAs xs ] -- is a (student) part incorrect?
   present = all (flip any bs . eq) as -- are all expected answers present
   notInAs = not . flip any as . eq
   partly  = length wrong < length ps
   noSplit = length as < 2 && length bs < 2 -}