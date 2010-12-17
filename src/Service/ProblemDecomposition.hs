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
-----------------------------------------------------------------------------
module Service.ProblemDecomposition 
   ( problemDecomposition
   , replyType, replyTypeSynonym, encodeReply
   ) where

import Common.Library
import Common.Utils
import Control.Monad
import Data.Maybe
import Service.ExercisePackage
import Service.State
import Service.Types
import Text.XML hiding (name)

problemDecomposition :: Monad m => Maybe Id -> State a -> Maybe a -> m (Reply a)
problemDecomposition msloc state answer 
   | isNothing $ subStrategy sloc (strategy ex) =
        fail "request error: invalid location for strategy"
   | otherwise =
   let pr = fromMaybe (emptyPrefix $ strategy ex) (statePrefix state) in
         case (runPrefixLocation sloc pr requestedTerm, maybe Nothing (Just . inContext ex) answer) of            
            ([], _) -> fail "strategy error: not able to compute an expected answer"
            (answers, Just answeredTerm)
               | not (null witnesses) -> return $
                    Ok newLocation newState
                  where 
                    witnesses   = filter (similarityCtx ex answeredTerm . fst) $ take 1 answers
                    (newCtx, newPrefix) = head witnesses
                    newLocation = nextTaskLocation (strategy ex) sloc $ 
                                     fromMaybe topId $ nextMajorForPrefix newPrefix newCtx
                    newState    = makeState pkg (Just newPrefix) newCtx
            ((expected, pref):_, maybeAnswer) -> return $
                    Incorrect isEquiv newLocation expState arguments
             where
               newLocation = subTaskLocation (strategy ex) sloc loc
               expState = makeState pkg (Just pref) expected
               isEquiv  = maybe False (equivalenceContext ex expected) maybeAnswer
               (loc, arguments) = fromMaybe (topId, []) $ 
                                     firstMajorInPrefix pr pref requestedTerm
 where
   pkg   = exercisePkg state
   ex    = exercise pkg
   topId = getId (strategy ex)
   sloc  = fromMaybe topId msloc
   requestedTerm = stateContext state
   
similarityCtx :: Exercise a -> Context a -> Context a -> Bool
similarityCtx ex a b = fromMaybe False $
   liftM2 (similarity ex) (fromContext a) (fromContext b)

-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: Id -> Prefix a -> a -> [(a, Prefix a)]
runPrefixLocation loc p0 =
   concatMap (checkPair . f) . derivations . 
   cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (Exit info)) = getId info == loc
   stop _ = False
 
   checkPair result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

firstMajorInPrefix :: Prefix a -> Prefix a -> a -> Maybe (Id, Args)
firstMajorInPrefix p0 p a = do
   let newSteps = drop (length $ prefixToSteps p0) (prefixToSteps p)
   is <- firstLocation newSteps
   return (is, argumentsForSteps a newSteps)
 where
   firstLocation :: HasId l => [Step l a] -> Maybe Id
   firstLocation [] = Nothing
   firstLocation (Enter info:RuleStep r:_) | isMajorRule r = Just (getId info)
   firstLocation (_:rest) = firstLocation rest
 
argumentsForSteps :: a -> [Step l a] -> Args
argumentsForSteps a0 = flip rec a0 . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = let ds = map (\(Some d) -> labelArgument d) (getDescriptors r)
                         in maybe [] (zip ds) (expectedArguments r a)
      | otherwise      = []
 
nextMajorForPrefix :: Prefix a -> a -> Maybe Id
nextMajorForPrefix p0 a = do
   (_, p1)  <- safeHead $ runPrefixMajor p0 a
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
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (RuleStep r)) = isMajorRule r
   stop _ = False
        
------------------------------------------------------------------------
-- Data types for replies

data Reply a = Ok Id (State a)
             | Incorrect Bool Id (State a) Args

type Args = [(String, String)]

------------------------------------------------------------------------
-- Conversion functions to XML

encodeReply :: Monad m => (State a -> m XMLBuilder) -> Reply a -> m XMLBuilder
encodeReply showState reply = 
   case reply of
      Ok loc state -> do
         stateXML <- showState state
         return $
            element "correct" $ do
               element "location" (text $ show loc)
               stateXML
      Incorrect b loc state args -> do 
         stateXML <- showState state 
         return $ 
            element "incorrect" $ do
               "equivalent" .=. show b
               element "location" (text $ show loc)
               stateXML
               let f (x, y) = element "elem" $ do 
                     "descr" .=. x 
                     text y
               unless (null args) $
                  element "arguments" $ mapM_ f args

replyType :: Type a (Reply a)
replyType = useSynonym replyTypeSynonym

replyTypeSynonym :: TypeSynonym a (Reply a)
replyTypeSynonym = typeSynonym "DecompositionReply" to from tp
 where
   to (Left (a, b))        = Ok a b
   to (Right (a, b, c, d)) = Incorrect a b c d
   
   from (Ok a b)            = Left (a, b)
   from (Incorrect a b c d) = Right (a, b, c, d)
   
   tp  =  tuple2 Id stateTp
      :|: tuple4 Bool Id stateTp argsTp

   argsTp = List (Pair String String)