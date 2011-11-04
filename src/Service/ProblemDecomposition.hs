-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.ProblemDecomposition
   ( problemDecomposition, replyType
   ) where

import Common.Library
import Data.Maybe
import Service.State
import Service.Types

problemDecomposition :: Maybe Id -> State a -> Maybe a -> Either String (Reply a)
problemDecomposition msloc state answer
   | isNothing $ subStrategy sloc (strategy ex) =
        Left "request error: invalid location for strategy"
   | otherwise =
   let pr = fromMaybe (emptyPrefix $ strategy ex) (statePrefix state) in
         case (runPrefixLocation sloc pr requestedTerm, fmap (inContext ex) answer) of
            ([], _) -> Left "strategy error: not able to compute an expected answer"
            (answers, Just answeredTerm)
               | not (null witnesses) -> Right $
                    Ok newLocation newState
                  where
                    witnesses   = filter (similarity ex answeredTerm . fst) $ take 1 answers
                    (newCtx, newPrefix) = head witnesses
                    newLocation = nextTaskLocation (strategy ex) sloc $
                                     fromMaybe topId $ nextMajorForPrefix newPrefix newCtx
                    newState    = makeState ex (Just newPrefix) newCtx
            ((expected, pref):_, maybeAnswer) -> Right $
                    Incorrect isEquiv newLocation expState arguments
             where
               newLocation = subTaskLocation (strategy ex) sloc loc
               expState = makeState ex (Just pref) expected
               isEquiv  = maybe False (equivalence ex expected) maybeAnswer
               (loc, arguments) = fromMaybe (topId, []) $
                                     firstMajorInPrefix pr pref requestedTerm
 where
   ex    = exercise state
   topId = getId (strategy ex)
   sloc  = fromMaybe topId msloc
   requestedTerm = stateContext state

-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: Id -> Prefix a -> a -> [(a, Prefix a)]
runPrefixLocation loc p0 =
   concatMap (checkPair . f) . derivations .
   cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (lastTerm d, fromMaybe p0 (lastStep d))
   stop (Just (Exit info)) = getId info == loc
   stop _ = False

   checkPair result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

firstMajorInPrefix :: Prefix a -> Prefix a -> a -> Maybe (Id, ArgValues)
firstMajorInPrefix p0 p a = do
   let newSteps = drop (length $ prefixToSteps p0) (prefixToSteps p)
   is <- firstLocation newSteps
   return (is, argumentsForSteps a newSteps)
 where
   firstLocation :: HasId l => [Step l a] -> Maybe Id
   firstLocation [] = Nothing
   firstLocation (Enter info:RuleStep r:_) | isMajorRule r = Just (getId info)
   firstLocation (_:rest) = firstLocation rest

argumentsForSteps :: a -> [Step l a] -> ArgValues
argumentsForSteps a0 = flip rec a0 . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = expectedArguments r a
      | otherwise      = []

nextMajorForPrefix :: Prefix a -> a -> Maybe Id
nextMajorForPrefix p0 a = do
   (_, p1)  <- listToMaybe $ runPrefixMajor p0 a
   rec (reverse (prefixToSteps p1))
 where
   rec [] = Nothing
   rec (Enter info:_) = Just (getId info)
   rec (Exit  info:_) = Just (getId info)
   rec (_:rest)       = rec rest

-- Copied from TypedAbstractService: clean me up
runPrefixMajor :: Prefix a -> a -> [(a, Prefix a)]
runPrefixMajor p0 =
   map f . derivations . cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (lastTerm d, fromMaybe p0 (lastStep d))
   stop (Just (RuleStep r)) = isMajorRule r
   stop _ = False

------------------------------------------------------------------------
-- Data types for replies

data Reply a = Ok Id (State a)
             | Incorrect Bool Id (State a) ArgValues

------------------------------------------------------------------------
-- Type definition

replyType :: Type a (Reply a)
replyType = Iso (f <-> g) tp
 where
   f (Left (a, b))        = Ok a b
   f (Right (a, b, c, d)) = Incorrect a b c d

   g (Ok a b)            = Left (a, b)
   g (Incorrect a b c d) = Right (a, b, c, d)

   tp  =  Tag "correct"   (tuple2 locType stateType)
      :|: Tag "incorrect" (tuple4 (Tag "equivalent" Bool) locType stateType argsType)

   locType  = Tag "location" Id
   argsType = List ArgValueTp