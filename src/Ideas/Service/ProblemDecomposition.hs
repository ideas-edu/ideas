{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------

module Ideas.Service.ProblemDecomposition
   ( problemDecomposition, Reply(..), Answer, tAnswer, tReply
   ) where

import Data.Maybe
import Ideas.Common.Library
import Ideas.Common.Strategy.Symbol
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Utils.Prelude (fst3)

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
              newState = state
                 { statePrefix  = newPrefix
                 , stateContext = newCtx
                 }
           _ -> Incorrect isEquiv newLocation expState arguments
            where
              newLocation = subTaskLocation strat sloc loc
              expState = state
                 { statePrefix  = pref
                 , stateContext = expected
                 }
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
runPrefixLocation :: Id -> Prefix a -> [(a, [(Rule a, Environment)], Prefix a)]
runPrefixLocation loc = rec []
 where
   rec acc p = do
      ((st, a, env), q) <- firsts p
      if isLoc st then return (a, reverse ((st, env):acc), q)
                  else rec ((st, env):acc) q

   isLoc r =
      case (isEnterRule r, isExitRule r) of
         (Just _, _) -> False
         (_, Just l) -> l == loc
         _           -> getId r == loc

firstMajorInSteps :: [(Rule a, Environment)] -> Maybe (Id, Environment)
firstMajorInSteps ((r, env):_) | isMajor r = Just (getId r, env)
firstMajorInSteps (_:xs) = firstMajorInSteps xs
firstMajorInSteps []     = Nothing

nextMajorForPrefix :: Prefix a -> Maybe Id
nextMajorForPrefix = listToMaybe . rec
 where
   rec prfx = do
      ((r, _, _), p) <- firsts prfx
      case isEnterRule r of
         Just l -> [l]
         Nothing
            | isMajor r -> [getId r]
            | otherwise -> rec p

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