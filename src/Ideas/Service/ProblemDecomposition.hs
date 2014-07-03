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
import Ideas.Common.Strategy.Parsing (firstsWith)
import Ideas.Service.State
import Ideas.Service.Types

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
                    witnesses   = filter (similarity ex answeredTerm . fst) $ take 1 answers
                    (newCtx, newPrefix) = head witnesses
                    newLocation = nextTaskLocation (strategy ex) sloc $
                                     fromMaybe topId $ nextMajorForPrefix newPrefix
                    newState    = makeState ex [newPrefix] newCtx
            _ -> Right $
                    Incorrect isEquiv newLocation expState arguments
             where
               newLocation = subTaskLocation (strategy ex) sloc loc
               expState = makeState ex [pref] expected
               isEquiv  = maybe False (equivalence ex expected . fromAnswer) maybeAnswer
               (expected, pref) = head answers
               (loc, arguments) = fromMaybe (topId, mempty) $
                                     firstMajorInPrefix prefix pref
 where
   ex      = exercise state
   topId   = getId (strategy ex)
   sloc    = fromMaybe topId msloc
   answers = runPrefixLocation sloc prefix
   prefix  = case statePrefixes state of
                []   -> emptyPrefix (strategy ex) (stateContext state)
                hd:_ -> hd

-- | Continue with a prefix until a certain strategy location is reached.
runPrefixLocation :: Id -> Prefix a -> [(a, Prefix a)]
runPrefixLocation loc p = do
   (a, q) <- firsts p
   case lastStepInPrefix q of
      Just (Exit info) | getId info == loc -> return (a, q)
      _ -> runPrefixLocation loc q

firstMajorInPrefix :: Prefix a -> Prefix a -> Maybe (Id, Environment)
firstMajorInPrefix p0 = rec . drop len . prefixToSteps
 where
   len = length (prefixToSteps p0)
   rec xs =
      case xs of
         Enter info:RuleStep env r:_ | isMajor r ->
            Just (getId info, env)
         _:rest -> rec rest
         []     -> Nothing

nextMajorForPrefix :: Prefix a -> Maybe Id
nextMajorForPrefix p0 = do
   (_, p1)  <- listToMaybe $ runPrefixMajor p0
   rec (reverse (prefixToSteps p1))
 where
   rec [] = Nothing
   rec (Enter info:_) = Just (getId info)
   rec (Exit  info:_) = Just (getId info)
   rec (_:rest)       = rec rest

-- Copied from TypedAbstractService: clean me up
runPrefixMajor :: Prefix a -> [(a, Prefix a)]
runPrefixMajor = firstsWith (const isMajor)

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