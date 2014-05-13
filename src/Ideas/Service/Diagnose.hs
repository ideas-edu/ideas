{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
module Ideas.Service.Diagnose
   ( Diagnosis(..), diagnose, restartIfNeeded, newState
   , difference, differenceEqual
   ) where

import Data.Function
import Data.List (sortBy)
import Data.Maybe
import Ideas.Common.Library hiding (ready)
import Ideas.Service.BasicServices hiding (apply)
import Ideas.Service.State
import Ideas.Service.Types
import qualified Ideas.Common.Rewriting.Difference as Diff

----------------------------------------------------------------
-- Result types for diagnose service

data Diagnosis a
   = Buggy          Environment (Rule (Context a))
--   | Missing
--   | IncorrectPart  [a]
   | NotEquivalent  String
   | Similar        Bool (State a)
   | WrongRule      Bool (State a) (Maybe (Rule (Context a)))
   | Expected       Bool (State a) (Rule (Context a))
   | Detour         Bool (State a) Environment (Rule (Context a))
   | Correct        Bool (State a)
   | Unknown        Bool (State a)  -- Added for the FP domain, to indicate that no
                                    -- diagnose is possible (i.e., QC gave up)

instance Show (Diagnosis a) where
   show diagnosis =
      case diagnosis of
         Buggy as r       -> "Buggy rule " ++ show (show r) ++ showArgs as
         Unknown _ _      -> "Unknown step"
--         Missing          -> "Missing solutions"
--         IncorrectPart xs -> "Incorrect parts (" ++ show (length xs) ++ " items)"
         NotEquivalent s  -> if null s then "Unknown mistake" else s
         Similar _ _      -> "Very similar"
         WrongRule _ _ mr -> "Wrong rule selected"  ++
                             maybe "" (\r -> ", " ++ showId r ++ "recognized") mr
         Expected _ _ r   -> "Rule " ++ show (show r) ++ ", expected by strategy"
         Detour _ _ _ r   -> "Rule " ++ show (show r) ++ ", not following strategy"
         Correct _ _      -> "Unknown step"
    where
      showArgs as
         | noBindings as = ""
         | otherwise     = " (" ++ show as ++ ")"

newState :: Diagnosis a -> Maybe (State a)
newState diagnosis =
   case diagnosis of
      Buggy _ _        -> Nothing
      NotEquivalent _  -> Nothing
      Similar  _ s     -> Just s
      WrongRule _ s _  -> Just s
      Expected _ s _   -> Just s
      Detour   _ s _ _ -> Just s
      Correct  _ s     -> Just s
      Unknown  _ s     -> Just s

----------------------------------------------------------------
-- The diagnose service

diagnose :: State a -> Context a -> Maybe Id -> Diagnosis a
diagnose state new motivationId
   -- Is the submitted term equivalent?
   | not (equivalence ex (stateContext state) new) =
        -- Is the rule used discoverable by trying all known buggy rules?
        case discovered True Nothing of
           Just (r, as) -> Buggy as r -- report the buggy rule
           Nothing      -> NotEquivalent "" -- compareParts state new

   -- Is the used rule that is submitted applied correctly?
   | isJust motivationId && isNothing (discovered False motivationId) =
        case discovered False Nothing of -- search for a "sound" rule
           Just (r, _) -> WrongRule (ready state) state (Just r)
           Nothing -> 
              case discovered True  Nothing of -- search for buggy rule
                 Just (r, as) -> 
                    Buggy as r -- report the buggy rule
                 Nothing ->
                    WrongRule (ready state) state Nothing

   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let ((r, _, _), ns) = fromJust expected
        in Expected (ready ns) ns r

   -- Is the submitted term (very) similar to the previous one?
   -- (this check is performed after "expected by strategy". TODO: fix
   -- granularity of some math rules)
   | similar = Similar (ready state) state

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        case discovered False Nothing of
           Just (r, as) ->  -- If yes, report the found rule as a detour
              Detour (ready restarted) restarted as r
           Nothing -> -- If not, we give up
              Correct (ready restarted) restarted
 where
   ex        = exercise state
   restarted = restartIfNeeded (makeNoState ex new)
   similar   = similarity ex (stateContext state) new

   expected = do
      let xs = either (const []) id $ allfirsts (restartIfNeeded state)
          p (_, ns) = similarity ex new (stateContext ns) -- use rule recognizer?
      listToMaybe (filter p xs)

   discovered searchForBuggy searchForRule = listToMaybe
      [ (r, env)
      | r <- sortBy (ruleOrdering ex) (ruleset ex)
      , isBuggy r == searchForBuggy
      , maybe True (`elem` getId r:ruleSiblings r) searchForRule
      , (_, env) <- recognizeRule ex r sub1 sub2
      ]
    where
      diff = if searchForBuggy then difference else differenceEqual
      (sub1, sub2) = fromMaybe (stateContext state, new) $ do
         newTerm <- fromContext new
         (a, b)  <- diff ex (stateTerm state) newTerm
         return (inContext ex a, inContext ex b)

----------------------------------------------------------------
-- Helpers

-- If possible (and if needed), restart the strategy
-- Make sure that the new state has a prefix
-- When resetting the prefix, also make sure that the context is refreshed
restartIfNeeded :: State a -> State a
restartIfNeeded state
   | null (statePrefixes state) && canBeRestarted ex =
        emptyState ex (stateTerm state)
   | otherwise = state
 where
   ex = exercise state

instance Typed a (Diagnosis a) where
   typed = Tag "Diagnosis" $ Iso (f <-> g) typed
    where
      f (Left (Left (as, r))) = Buggy as r
   --   f (Left (Right (Left ()))) = Missing
   --   f (Left (Right (Right (Left xs)))) = IncorrectPart xs
      f (Left (Right (Left s))) = NotEquivalent s
      f (Left (Right (Right (b, s, mr)))) = WrongRule b s mr
      f (Right (Left (b, s))) = Similar b s
      f (Right (Right (Left (b, s, r)))) = Expected b s r
      f (Right (Right (Right (Left (b, s, as, r))))) = Detour b s as r
      f (Right (Right (Right (Right (Left (b, s)))))) = Correct b s
      f (Right (Right (Right (Right (Right (b, s)))))) = Unknown b s

      g (Buggy as r)       = Left (Left (as, r))
   --   g Missing            = Left (Right (Left ()))
   --   g (IncorrectPart xs) = Left (Right (Right (Left xs)))
      g (NotEquivalent s)  = Left (Right (Left s))
      g (WrongRule b s mr) = Left (Right (Right (b, s, mr)))
      g (Similar b s)      = Right (Left (b, s))
      g (Expected b s r)   = Right (Right (Left (b, s, r)))
      g (Detour b s as r)  = Right (Right (Right (Left (b, s, as, r))))
      g (Correct b s)      = Right (Right (Right (Right (Left (b, s)))))
      g (Unknown b s)      = Right (Right (Right (Right (Right (b, s)))))

difference :: Exercise a -> a -> a -> Maybe (a, a)
difference ex a b = do
   v <- hasTermView ex
   Diff.differenceWith v a b

-- Used by the FP tutor
differenceEqual :: Exercise a -> a -> a -> Maybe (a, a)
differenceEqual ex a b = do
   v <- hasTermView ex
   let simpleEq = equivalence ex `on` inContext ex
   Diff.differenceEqualWith v simpleEq a b

----------------------------------------------------------------
-- Compare answer sets (and search for missing parts/incorrect parts)
{-  splitParts     :: a -> [a]
compareParts :: State a -> a -> Diagnosis a
compareParts state = answerList eq split solve (stateTerm state)
 where
   ex    = exercise (exercise state)
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