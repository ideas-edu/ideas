module OpenMath.LAServer (respond, laServerFor, versionNr) where

import Domain.LinearAlgebra
import OpenMath.StrategyTable
import OpenMath.Request
import OpenMath.Reply
import OpenMath.ObjectParser
import Common.Context
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Assignment hiding (Incorrect)
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
   case [ ea | Entry _ ea@(ExprAssignment a) _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [ExprAssignment a] -> laServerFor a req
      _ -> replyError "request error" "unknown strategy"
   
laServerFor :: IsExpr a => Assignment (Context a) -> Request -> Reply
laServerFor a req = 
   case (subStrategy (req_Location req) (strategy a), getContextTerm req) of
   
      (Nothing, _) -> 
         replyError "request error" "invalid location for strategy"
         
      (_, Nothing) ->
         replyError "request error" ("invalid term for " ++ show (req_Strategy req))
         
      (Just subStrategy, Just requestedTerm) -> 
         
         case (runStrategyUntil (req_Location req) requestedTerm (strategy a), maybe Nothing (fmap inContext . fromExpr) $ req_Answer req) of
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok $ ReplyOk
                       { repOk_Strategy = req_Strategy req
                       , repOk_Location = nextTask (req_Location req) $ nextMajorForPrefix (snd $ head $ witnesses) (fst $ head $ witnesses) (strategy a)
                       , repOK_Context  = "" -- showContext newContext
                       , repOk_Steps    = 0 -- stepsRemaining (unlabel $ strategy a) (answeredTerm) -- not precise
                       }
                  where
                    witnesses = filter (equality a answeredTerm . fst) answers
                    --(loc, newContext) = nextLocation answeredTerm (req_Location req) a
                       
            ((expected,prefix):_, maybeAnswer) ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = subTask (req_Location req) $ firstMajorInPrefix prefix (strategy a)
                       
                       --req_Location req ++ take 1 subloc
                      -- , repInc_Context    = showContext newContext
                       , repInc_Expected   = toExpr (fromContext expected)
                       , repInc_Steps      = 0 -- stepsRemaining (unlabel $ strategy a) requestedTerm -- not precise
                       , repInc_Equivalent = maybe False (equivalence a expected) maybeAnswer
                       }

-- old (current) and actual (next major rule) location
subTask :: [Int] -> [Int] -> [Int]
subTask (i:is) (j:js)
   | i == j    = i : subTask is js
   | otherwise = []
subTask _ js   = take 1 js

-- old (current) and actual (next major rule) location
nextTask :: [Int] -> [Int] -> [Int]
nextTask (i:is) (j:js)
   | i == j   = i : nextTask is js
nextTask _ js = take 1 js

runStrategyUntil :: [Int] -> a -> LabeledStrategy a -> [(a, Prefix)]
runStrategyUntil loc a s = runGrammarUntil stopC a (withMarks s)
 where
   stopC (End is) = is==loc
   _              = False
   
firstMajorInPrefix :: Prefix -> LabeledStrategy a -> [Int]
firstMajorInPrefix p s = maybe [] (f [[]]) (prefixToSteps p s)
 where
   f stack@(hd:tl) (step:rest) = 
      case step of
         Major _  -> hd
         Begin is -> f (is:stack) rest
         End _    -> f tl rest
         _        -> f stack rest
   f _ _ = []
   
nextMajorForPrefix :: Prefix -> a -> LabeledStrategy a -> [Int]
nextMajorForPrefix p0 a s = 
   case runPrefix p0 s of 
      Just (steps, g) -> 
         case runGrammarUntil stopC a g of
            (a, p1):_ -> maybe [] (f [[]]) (prefixToSteps (plusPrefix p0 p1) s)
            [] -> []
      Nothing -> []
 where
   stopC (Major _) = True
   stopC _         = False

   f (hd:_) [] = hd
   f stack@(hd:tl) (step:rest) = 
      case step of
         Begin is -> f (is:stack) rest
         End _    -> f tl rest
         _        -> f stack rest
   f _ _ = []