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
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Service.ProblemDecomposition
   ( problemDecomposition, Reply(..)
   ) where

import Data.Maybe
import Ideas.Common.Library
import Ideas.Service.State
import Ideas.Service.Types
import Ideas.Common.Utils (fst3)

problemDecomposition :: Maybe Id -> State a -> Maybe (Answer a) -> Either String (Reply a)
problemDecomposition msloc state maybeAnswer
   | isNothing $ subStrategy sloc (strategy ex) =
        Left "request error: invalid location for strategy"
   | null answers =
        Left "strategy error: not able to compute an expected answer"
   | otherwise =
         case maybeAnswer of
            Just (Answer answeredTerm) | not (null witnesses) -> Right $
                    Ok newLocation newState
                  where
                    witnesses   = filter (similarity ex answeredTerm . fst3) $ take 1 answers
                    (newCtx, _, newPrefix) = head witnesses
                    newLocation = nextTaskLocation (strategy ex) sloc $
                                     fromMaybe topId $ nextMajorForPrefix newPrefix
                    newState    = makeState ex [newPrefix] newCtx
            _ -> Right $
                    Incorrect isEquiv newLocation expState arguments
             where
               newLocation = subTaskLocation (strategy ex) sloc loc
               expState = makeState ex [pref] expected
               isEquiv  = maybe False (equivalence ex expected . fromAnswer) maybeAnswer
               (expected, answerSteps, pref) = head answers
               (loc, arguments) = fromMaybe (topId, mempty) $
                                     firstMajorInSteps answerSteps --  prefix pref
 where
   ex      = exercise state
   topId   = getId (strategy ex)
   sloc    = fromMaybe topId msloc
   answers = runPrefixLocation sloc prefix
   prefix  = case statePrefixes state of
                []   -> emptyPrefix (strategy ex) (stateContext state)
                hd:_ -> hd

-- | Continue with a prefix until a certain strategy location is reached.
runPrefixLocation :: Id -> Prefix a -> [(a, [Step a], Prefix a)]
runPrefixLocation loc = rec []
 where
   rec acc p = do
      ((stp, a), q) <- firsts p
      case stp of
         Exit l | l == loc -> return (a, reverse acc, q)
         _ -> rec (stp:acc) q

firstMajorInSteps :: [Step a] -> Maybe (Id, Environment)
firstMajorInSteps xs =
   case xs of
      Enter l:RuleStep env r:_ | isMajor r ->
         Just (l, env)
      _:rest -> firstMajorInSteps rest
      []     -> Nothing

nextMajorForPrefix :: Prefix a -> Maybe Id
nextMajorForPrefix = listToMaybe . rec []
 where
   rec res prfx = do
      ((stp, _), p) <- firsts prfx
      case stp of 
         Enter l -> rec [l] p
         Exit l  -> rec [l] p
         RuleStep _ r 
            | isMajor r -> res
            | otherwise -> rec res p

------------------------------------------------------------------------
-- Data types for replies

newtype Answer a = Answer { fromAnswer :: Context a }

data Reply a = Ok Id (State a)
             | Incorrect Bool Id (State a) Environment

------------------------------------------------------------------------
-- Type definition

instance Typed a (Answer a) where
   typed = Tag "answer" $ Iso (Answer <-> fromAnswer) (Const Context)

instance Typed a (Reply a) where
   typed = Tag "DecompositionReply" (Iso (f <-> g) typed)
    where
      f (Left (a, b))        = Ok a b
      f (Right (a, b, c, d)) = Incorrect a b c d

      g (Ok a b)            = Left (a, b)
      g (Incorrect a b c d) = Right (a, b, c, d)