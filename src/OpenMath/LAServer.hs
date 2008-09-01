-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.LAServer (respond, laServer, laServerFor, versionNr) where

import OpenMath.StrategyTable
import OpenMath.Request
import OpenMath.Reply
import OpenMath.Conversion
import Common.Apply
import Common.Context
import Common.Transformation
import Common.Strategy hiding (not, repeat)
import Common.Exercise hiding (Incorrect)
import Common.Utils
import Data.Maybe
import Data.Char
import Data.List

respond :: Maybe String -> String
respond = replyInXML . maybe requestError (either parseError laServer . pRequest)

replyError :: String -> String -> Reply
replyError kind = Error . ReplyError kind

parseError :: String -> Reply
parseError   = replyError "parse error"

requestError :: Reply
requestError = replyError "request error" "no request found in \"input\""

(~=) :: String -> String -> Bool
xs ~= ys = let f = map toLower . filter isAlphaNum
           in f xs == f ys 

laServer :: Request -> Reply
laServer req = 
   case [ ea | Entry _ ea@(Some (ExprExercise a)) _ _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [Some (ExprExercise a)] -> laServerFor a req
      _ -> replyError "request error" "unknown strategy"
   
laServerFor :: IsOMOBJ a => Exercise (Context a) -> Request -> Reply
laServerFor a req = 
   case getContextTerm req of
   
      _ | isNothing $ subStrategy (req_Location req) (strategy a) ->
             replyError "request error" "invalid location for strategy"
         
      Nothing ->
         replyError "request error" ("invalid term for " ++ show (req_Strategy req))
         
      Just requestedTerm ->          
         case (runPrefixLocation (req_Location req) (getPrefix req (strategy a)) requestedTerm, maybe Nothing (fmap inContext . fromOMOBJ) $ req_Answer req) of
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok $ ReplyOk
                       { repOk_Strategy = req_Strategy req
                       , repOk_Location = nextTask (req_Location req) $ nextMajorForPrefix newPrefix (fst $ head witnesses)
                       , repOk_Context  = show newPrefix ++ ";" ++ 
                                          showContext (fst $ head witnesses)
                       , repOk_Steps    = stepsRemaining newPrefix (fst $ head witnesses)
                       }
                  where
                    witnesses   = filter (equality a answeredTerm . fst) answers
                    newPrefix   = snd (head witnesses)
                       
            ((expected, prefix):_, maybeAnswer) ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = subTask (req_Location req) loc
                       , repInc_Expected   = toOMOBJ (fromContext expected)
                       , repInc_Derivation = derivation
                       , repInc_Arguments  = args
                       , repInc_Steps      = stepsRemaining (getPrefix req (strategy a)) requestedTerm
                       , repInc_Equivalent = maybe False (equivalence a expected) maybeAnswer
                       }
             where
               (loc, args) = firstMajorInPrefix (getPrefix req (strategy a)) prefix requestedTerm
               derivation  = 
                  let len      = length $ prefixToSteps $ getPrefix req $ strategy a
                      rules    = stepsToRules $ drop len $ prefixToSteps prefix
                      f (s, a) = (s, toOMOBJ $ fromContext a)
                  in map f (makeDerivation requestedTerm rules)

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