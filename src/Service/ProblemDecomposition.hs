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
   , Reply(..), replyToXML
   , ReplyIncorrect(..)
   , replyType, replyTypeSynonym
   ) where

import Common.Classes
import Common.Context
import Common.Derivation
import Common.Exercise
import Common.Strategy hiding (not, repeat, fail)
import Common.Transformation 
import Common.Utils
import Control.Monad
import Data.Maybe
import Service.BasicServices (stepsremaining)
import Service.ExercisePackage
import Service.State
import Service.Types
import Text.OpenMath.Object
import Text.XML hiding (name)

problemDecomposition :: Monad m => StrategyLocation -> State a -> Maybe a -> m (Reply a)
problemDecomposition sloc (State pkg mpr requestedTerm) answer 
   | isNothing $ subStrategy sloc (strategy ex) =
        fail "request error: invalid location for strategy"
   | otherwise =
   let pr = fromMaybe (emptyPrefix $ strategy ex) mpr in
         case (runPrefixLocation ex sloc pr requestedTerm, maybe Nothing (Just . inContext ex) answer) of            
            ([], _) -> fail "strategy error: not able to compute an expected answer"
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    return $ Ok ReplyOk
                       { repOk_Location = nextTaskLocation sloc $ nextMajorForPrefix ex newPrefix (fst $ head witnesses)
                       , repOk_Context  = show newPrefix ++ ";" ++ 
                                          show (getEnvironment $ fst $ head witnesses)
                       , repOk_Steps    = fromMaybe 0 $ stepsremaining $ State pkg (Just newPrefix) (fst $ head witnesses)
                       }
                  where 
                    witnesses   = filter (similarityCtx ex answeredTerm . fst) $ take 1 answers
                    newPrefix   = snd (head witnesses)            
            ((expected, prefix):_, maybeAnswer) ->
                    return $ Incorrect ReplyIncorrect
                       { repInc_Location   = subTaskLocation sloc loc
                       , repInc_Expected   = fromJust (fromContext expected)
                       , repInc_Derivation = derivation
                       , repInc_Arguments  = args
                       , repInc_Steps      = fromMaybe 0 $ stepsremaining $ State pkg (Just pr) requestedTerm
                       , repInc_Equivalent = maybe False (equivalenceContext ex expected) maybeAnswer
                       }
             where
               (loc, args) = firstMajorInPrefix ex pr prefix requestedTerm
               derivation  = 
                  let len      = length $ prefixToSteps pr
                      rules    = stepsToRules $ drop len $ prefixToSteps prefix
                      f (s, a) = (s, fromJust (fromContext a))
                  in map f (makeDerivation requestedTerm rules)
 where
   ex = exercise pkg
   
similarityCtx :: Exercise a -> Context a -> Context a -> Bool
similarityCtx ex a b = fromMaybe False $
   liftM2 (similarity ex) (fromContext a) (fromContext b)

-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: Exercise a -> StrategyLocation -> Prefix (Context a) -> Context a -> [(Context a, Prefix (Context a))]
runPrefixLocation ex loc p0 = -- type variable specialized to context due to exercise
   concatMap (check . f) . derivations . 
   cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (Exit info)) = maybe False (getId info ==) $ locationToId (strategy ex) loc
   stop _ = False
 
   check result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation ex loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

firstMajorInPrefix :: Exercise a -> Prefix (Context a) -> Prefix (Context a) -> Context a -> (StrategyLocation, Args)
firstMajorInPrefix ex p0 prefix a = fromMaybe (topLocation, []) $ do -- type variable specialized to context due to exercise
   let steps = prefixToSteps prefix
       newSteps = drop (length $ prefixToSteps p0) steps
   is <- firstLocation newSteps
   return (is, argumentsForSteps a newSteps)
 where
   firstLocation :: HasId l => [Step l a] -> Maybe StrategyLocation
   firstLocation [] = Nothing
   firstLocation (Enter info:RuleStep r:_) | isMajorRule r = idToLocation (strategy ex) (getId info)
   firstLocation (_:rest) = firstLocation rest
 
argumentsForSteps :: a -> [Step l a] -> Args
argumentsForSteps a = flip rec a . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = let ds = map (\(Some d) -> labelArgument d) (getDescriptors r)
                         in maybe [] (zip ds) (expectedArguments r a)
      | otherwise      = []
 
nextMajorForPrefix :: Exercise a -> Prefix (Context a) -> Context a -> StrategyLocation
nextMajorForPrefix ex p0 a = fromMaybe topLocation $ do -- type variable specialized to context due to exercise
   (_, p1)  <- safeHead $ runPrefixMajor p0 a
   let steps = prefixToSteps p1
   rec (reverse steps)
 where
   rec [] = Nothing
   rec (Enter info:_) = idToLocation (strategy ex) (getId info)
   rec (Exit  info:_) = idToLocation (strategy ex) (getId info)
   rec (_:rest)       = rec rest
  
makeDerivation :: a -> [Rule a] -> [(String, a)]
makeDerivation _ []     = []
makeDerivation a (r:rs) = 
   let new = applyD r a
   in [ (showId r, new) | isMajorRule r ] ++ makeDerivation new rs 
   
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

data Reply a = Ok (ReplyOk a) | Incorrect (ReplyIncorrect a)

data ReplyOk a = ReplyOk
   { repOk_Location :: StrategyLocation
   , repOk_Context  :: String
   , repOk_Steps    :: Int
   }
   
data ReplyIncorrect a = ReplyIncorrect
   { repInc_Location   :: StrategyLocation
   , repInc_Expected   :: a
   , repInc_Derivation :: [(String, a)]
   , repInc_Arguments  :: Args
   , repInc_Steps      :: Int
   , repInc_Equivalent :: Bool
   }

type Args = [(String, String)]

------------------------------------------------------------------------
-- Conversion functions to XML

replyToXML :: (a -> OMOBJ) -> Reply a -> XMLBuilder
replyToXML toOpenMath reply =
   case reply of
      Ok r        -> replyOkToXML r
      Incorrect r -> replyIncorrectToXML toOpenMath r 

replyOkToXML :: ReplyOk a -> XMLBuilder
replyOkToXML r = element "correct" $ do
   element "location" (text $ show $ repOk_Location r)
   element "context"  (text $ repOk_Context r)
   element "steps"    (text $ show $ repOk_Steps r)

replyIncorrectToXML :: (a -> OMOBJ) -> ReplyIncorrect a -> XMLBuilder
replyIncorrectToXML toOpenMath r = element "incorrect" $ do
   element "location"   (text $ show $ repInc_Location r)
   element "expected"   (builder $ omobj2xml $ toOpenMath $ repInc_Expected r)
   element "steps"      (text $ show $ repInc_Steps r)
   element "equivalent" (text $ show $ repInc_Equivalent r)
   
   unless (null $ repInc_Arguments r) $
       let f (x, y) = element "elem" $ do 
              "descr" .=. x 
              text y
       in element "arguments" $ mapM_ f (repInc_Arguments r)

   unless (null $  repInc_Derivation r) $
      let f (x,y) = element "elem" $ do 
             "ruleid" .=. x 
             builder (omobj2xml (toOpenMath y))
      in element "derivation" $ mapM_ f (repInc_Derivation r)

replyType :: Type a (Reply a)
replyType = useSynonym replyTypeSynonym

replyTypeSynonym :: TypeSynonym a (Reply a)
replyTypeSynonym = typeSynonym "DecompositionReply" to from tp
 where
   to (Left (a, b, c)) = 
      Ok (ReplyOk a b c)
   to (Right ((a, b, c), (d, e, f))) =
      Incorrect (ReplyIncorrect a b c d e f)
   
   from (Ok (ReplyOk a b c)) = Left (a, b, c)
   from (Incorrect (ReplyIncorrect a b c d e f)) =
      Right ((a, b, c), (d, e, f))
   
   tp  =  tuple3 StrategyLoc String Int
      :|: Pair (tuple3 StrategyLoc Term derTp) 
               (tuple3 argsTp Int Bool)

   derTp  = List (Pair String Term)
   argsTp = List (Pair String String)