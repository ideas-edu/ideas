{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
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
   ( Diagnosis(..), tDiagnosis, diagnose
   , difference, differenceEqual
   ) where

import Data.Function
import Data.List (intercalate, sortBy)
import Data.Maybe
import Ideas.Common.Library hiding (ready)
import Ideas.Service.BasicServices hiding (apply)
import Ideas.Service.State
import Ideas.Service.Types
import qualified Ideas.Common.Rewriting.Difference as Diff

----------------------------------------------------------------
-- Result types for diagnose service

data Diagnosis a
   = SyntaxError    String
   | Buggy          Environment (Rule (Context a))
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
         SyntaxError s    -> f "SyntaxError" [s]
         Buggy _ r        -> f "Buggy" [show r]
         NotEquivalent s  -> f "NotEquivalent" [ s | not (null s) ]
         Similar _ _      -> "Similar"
         WrongRule _ _ mr -> f "WrongRule" [ show r | r <- maybeToList mr ]
         Expected _ _ r   -> f "Expected" [show r]
         Detour _ _ _ r   -> f "Detour" [show r]
         Correct _ _      -> "Correct"
         Unknown _ _      -> "Unknown"
    where
      f s xs
         | null xs   = s
         | otherwise = s ++ "(" ++ intercalate "," xs ++ ")"
{-
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
-}
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
           Just (r, _) -> WrongRule (finished state) state (Just r)
           Nothing ->
              case discovered True  Nothing of -- search for buggy rule
                 Just (r, as) ->
                    Buggy as r -- report the buggy rule
                 Nothing ->
                    WrongRule (finished state) state Nothing

   -- Was the submitted term expected by the strategy?
   | isJust expected =
        -- If yes, return new state and rule
        let ((r, _, _), ns) = fromJust expected
        in Expected (finished ns) ns r

   -- Is the submitted term (very) similar to the previous one?
   -- (this check is performed after "expected by strategy". TODO: fix
   -- granularity of some math rules)
   | similar = Similar (finished state) state

   -- Is the rule used discoverable by trying all known rules?
   | otherwise =
        case discovered False Nothing of
           Just (r, as) ->  -- If yes, report the found rule as a detour
              Detour (finished restarted) restarted as r
           Nothing -> -- If not, we give up
              Correct (finished restarted) restarted
 where
   ex        = exercise state
   restarted = restart state {stateContext = new}
   similar   = similarity ex (stateContext state) new

   expected = do
      let xs = either (const []) id $ allfirsts state
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

tDiagnosis :: Type a (Diagnosis a)
tDiagnosis = Tag "Diagnosis" $ Iso (f <-> g) tp
    where
      tp = (tString :|: tPair tEnvironment tRule :|: (tString :|: tTuple3 tBool tState (tMaybe tRule)))
         :|: tPair tBool tState :|: tTuple3 tBool tState tRule
         :|: tTuple4 tBool tState tEnvironment tRule :|: tPair tBool tState :|: tPair tBool tState

      f (Left (Left s)) = SyntaxError s
      f (Left (Right (Left (as, r)))) = Buggy as r
      f (Left (Right (Right (Left s)))) = NotEquivalent s
      f (Left (Right (Right (Right (b, s, mr))))) = WrongRule b s mr
      f (Right (Left (b, s))) = Similar b s
      f (Right (Right (Left (b, s, r)))) = Expected b s r
      f (Right (Right (Right (Left (b, s, as, r))))) = Detour b s as r
      f (Right (Right (Right (Right (Left (b, s)))))) = Correct b s
      f (Right (Right (Right (Right (Right (b, s)))))) = Unknown b s

      g (SyntaxError s)    = Left (Left s)
      g (Buggy as r)       = Left (Right (Left (as, r)))
      g (NotEquivalent s)  = Left (Right (Right (Left s)))
      g (WrongRule b s mr) = Left (Right (Right (Right (b, s, mr))))
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