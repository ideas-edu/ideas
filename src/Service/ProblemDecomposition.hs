-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.ProblemDecomposition (problemDecomposition) where

import Common.Apply
import Common.Context
import Common.Exercise
import Common.Derivation
import Common.Strategy hiding (not, repeat)
import Common.Transformation
import Common.Utils
import Data.Char
import Data.List
import Data.Maybe
import Text.OpenMath.Reply
import Service.TypedAbstractService (State(..), stepsremaining)

replyError :: String -> String -> Reply a
replyError kind = Error . ReplyError kind

problemDecomposition :: State a -> StrategyLocation -> Maybe a -> Reply a
problemDecomposition st@(State ex mpr requestedTerm) sloc answer 
   | isNothing $ subStrategy sloc (strategy ex) =
        replyError "request error" "invalid location for strategy"
   | otherwise =
   let pr = fromMaybe (emptyPrefix $ strategy ex) mpr in
         case (runPrefixLocation sloc pr requestedTerm, maybe Nothing (Just . inContext) answer) of            
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok ReplyOk
                       { repOk_Code     = ex
                       , repOk_Location = nextTask sloc $ nextMajorForPrefix newPrefix (fst $ head witnesses)
                       , repOk_Context  = show newPrefix ++ ";" ++ 
                                          show (getEnvironment $ fst $ head witnesses)
                       , repOk_Steps    = stepsremaining $ State ex (Just newPrefix) (fst $ head witnesses)
                       }
                  where 
                    witnesses   = filter (similarity ex (fromContext answeredTerm) . fromContext . fst) $ take 1 answers
                    newPrefix   = snd (head witnesses)
                      
            ((expected, prefix):_, maybeAnswer) ->
                    Incorrect ReplyIncorrect
                       { repInc_Code       = ex
                       , repInc_Location   = subTask sloc loc
                       , repInc_Expected   = fromContext expected
                       , repInc_Derivation = derivation
                       , repInc_Arguments  = args
                       , repInc_Steps      = stepsremaining $ State ex (Just pr) requestedTerm
                       , repInc_Equivalent = maybe False (equivalence ex (fromContext expected) . fromContext) maybeAnswer
                       }  
             where
               (loc, args) = firstMajorInPrefix pr prefix requestedTerm
               derivation  = 
                  let len      = length $ prefixToSteps pr
                      rules    = stepsToRules $ drop len $ prefixToSteps prefix
                      f (s, a) = (s, fromContext a)
                  in map f (makeDerivation requestedTerm rules)

-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: StrategyLocation -> Prefix a -> a -> [(a, Prefix a)]
runPrefixLocation loc p0 = 
   concatMap (check . f) . derivations . 
   cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (End is))    = is==loc
   stop (Just (Step is _)) = is==loc
   stop _ = False
 
   check result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

-- old (current) and actual (next major rule) location
subTask :: [Int] -> [Int] -> [Int]
subTask (i:is) (j:js)
   | i == j    = i : subTask is js
   | otherwise = []
subTask _ js   = take 1 js

-- old (current) and actual (next major rule) location
nextTask :: [Int] -> [Int] -> [Int]
nextTask (i:is) (j:js)
   | i == j    = i : nextTask is js
   | otherwise = [j] 
nextTask _ _   = [] 

firstMajorInPrefix :: Prefix a -> Prefix a -> a -> ([Int], Args)
firstMajorInPrefix p0 prefix a = fromMaybe ([], []) $ do
   let steps = prefixToSteps prefix
       newSteps = drop (length $ prefixToSteps p0) steps
   is    <- safeHead [ is | Step is r <- newSteps, isMajorRule r ]
   return (is, argumentsForSteps a newSteps)
 
argumentsForSteps :: a -> [Step a] -> Args
argumentsForSteps a = flip rec a . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = let ds = map (\(Some d) -> labelArgument d) (getDescriptors r)
                         in maybe [] (zip ds) (expectedArguments r a)
      | otherwise      = []
 
nextMajorForPrefix :: Prefix a -> a -> [Int]
nextMajorForPrefix p0 a = fromMaybe [] $ do
   (_, p1)  <- safeHead $ runPrefixMajor p0 a
   let steps = prefixToSteps p1
   lastStep <- safeHead (reverse steps)
   case lastStep of
      Step is r | not (isMinorRule r) -> return is
      _ -> Nothing

makeDerivation :: a -> [Rule a] -> [(String, a)]
makeDerivation _ []     = []
makeDerivation a (r:rs) = 
   let new = applyD r a
   in [ (name r, new) | isMajorRule r ] ++ makeDerivation new rs 
   
-- Copied from TypedAbstractService: clean me up
runPrefixMajor :: Prefix a -> a -> [(a, Prefix a)]
runPrefixMajor p0 = 
   map f . derivations . cutOnStep (stop . lastStepInPrefix) . prefixTree p0
 where
   f d = (last (terms d), if isEmpty d then p0 else last (steps d))
   stop (Just (Step _ r)) = isMajorRule r
   stop _ = False