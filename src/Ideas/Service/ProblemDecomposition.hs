{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Service.ProblemDecomposition
   ( problemDecomposition, Reply(..), Answer, tAnswer, tReply
   ) where

import Data.Maybe
import Ideas.Common.Library
import Ideas.Common.Utils (fst3)
import Ideas.Service.State
import Ideas.Service.Types

problemDecomposition :: Maybe Id -> State a -> Maybe (Answer a) -> Either String (Reply a)
problemDecomposition msloc state maybeAnswer
   | not (checkLocation sloc strat) =
        Left "request error: invalid location for strategy"
   | null answers =
        Left "strategy error: not able to compute an expected answer"
   | otherwise = Right $
        case maybeAnswer of

           Just (Answer answeredTerm) | not (null witnesses) ->
              Ok newLocation newState
            where
              witnesses = filter (similarity ex answeredTerm . fst3) $ take 1 answers
              (newCtx, _, newPrefix) = head witnesses
              newLocation = nextTaskLocation strat sloc $
                               fromMaybe topId $ nextMajorForPrefix newPrefix
              newState = makeState ex newPrefix newCtx

           _ -> Incorrect isEquiv newLocation expState arguments
            where
              newLocation = subTaskLocation strat sloc loc
              expState = makeState ex pref expected
              isEquiv  = maybe False (equivalence ex expected . fromAnswer) maybeAnswer
              (expected, answerSteps, pref) = head answers
              (loc, arguments) = fromMaybe (topId, mempty) $
                                    firstMajorInSteps answerSteps
 where
   ex      = exercise state
   strat   = strategy ex
   topId   = getId strat
   sloc    = fromMaybe topId msloc
   answers = runPrefixLocation sloc prefix
   prefix
      | withoutPrefix state = emptyPrefix strat (stateContext state)
      | otherwise           = statePrefix state

-- | Continue with a prefix until a certain strategy location is reached.
runPrefixLocation :: Id -> Prefix a -> [(a, [Step a], Prefix a)]
runPrefixLocation loc = rec []
 where
   rec acc p = do
      ((st, a), q) <- firsts p
      if isLoc st then return (a, reverse (st:acc), q)
                  else rec (st:acc) q

   isLoc (Exit l)       = l       == loc
   isLoc (RuleStep _ r) = getId r == loc
   isLoc _ = False

firstMajorInSteps :: [Step a] -> Maybe (Id, Environment)
firstMajorInSteps (RuleStep env r:_) | isMajor r = Just (getId r, env)
firstMajorInSteps (_:xs) = firstMajorInSteps xs
firstMajorInSteps []     = Nothing

nextMajorForPrefix :: Prefix a -> Maybe Id
nextMajorForPrefix = listToMaybe . rec
 where
   rec prfx = do
      ((st, _), p) <- firsts prfx
      case st of
         Enter l -> [l]
         RuleStep _ r | isMajor r -> [getId r]
         _ -> rec p

------------------------------------------------------------------------
-- Data types for replies

newtype Answer a = Answer { fromAnswer :: Context a }

data Reply a = Ok Id (State a)
             | Incorrect Bool Id (State a) Environment

------------------------------------------------------------------------
-- Type definition

tAnswer :: Type a (Answer a)
tAnswer = Tag "answer" $ Iso (Answer <-> fromAnswer) (Const Context)

tReply :: Type a (Reply a)
tReply = Tag "DecompositionReply" (Iso (f <-> g) tp)
    where
      tp = tPair tId tState :|: tTuple4 tBool tId tState tEnvironment

      f (Left (a, b))        = Ok a b
      f (Right (a, b, c, d)) = Incorrect a b c d

      g (Ok a b)            = Left (a, b)
      g (Incorrect a b c d) = Right (a, b, c, d)