module OpenMath.LAServer (respond, laServerFor, versionNr) where

import Domain.LinearAlgebra
import OpenMath.StrategyTable
import OpenMath.Request
import OpenMath.Reply
import OpenMath.ObjectParser
import Common.Context
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Assignment hiding (Incorrect, stepsRemaining)
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
   case [ ea | Entry _ ea@(ExprAssignment a) _ _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [ExprAssignment a] -> laServerFor a req
      _ -> replyError "request error" "unknown strategy"
   
laServerFor :: IsExpr a => Assignment (Context a) -> Request -> Reply
laServerFor a req = 
   case (subStrategy (req_Location req) (strategy a), getContextTerm req) of
   
      (Nothing, _) -> 
         replyError "request error" "invalid location for strategy"
         
      (_, Nothing) ->
         replyError "request error" ("invalid term for " ++ show (req_Strategy req))
         
      (Just _, Just requestedTerm) ->          
         case (runStrategyUntil (req_Location req) (getPrefix req) requestedTerm (strategy a), maybe Nothing (fmap inContext . fromExpr) $ req_Answer req) of
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok $ ReplyOk
                       { repOk_Strategy = req_Strategy req
                       , repOk_Location = nextTask (req_Location req) $ nextMajorForPrefix newPrefix (fst $ head witnesses) (strategy a)
                       , repOk_Context  = show newPrefix ++ ";" ++ 
                                          showContext (fst $ head witnesses)
                       , repOk_Steps    = stepsRemaining newPrefix (fst $ head witnesses) (strategy a)
                       }
                  where
                    witnesses = filter (equality a answeredTerm . fst) answers
                    newPrefix = getPrefix req `plusPrefix` snd (head witnesses)
                       
            ((expected,prefix):_, maybeAnswer) ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = subTask (req_Location req) $ firstMajorInPrefix (getPrefix req) prefix (strategy a)
                       , repInc_Expected   = toExpr (fromContext expected)
                       , repInc_Steps      = stepsRemaining (getPrefix req) requestedTerm (strategy a)
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

stepsRemaining :: Prefix -> a -> LabeledStrategy a -> Int
stepsRemaining p0@(P xs) a s = 
   case runStrategyUntil [] p0 a s of -- run until the end
      []       -> 0
      (_, p):_ ->
         case prefixToSteps (plusPrefix p0 p) s of
            Just steps -> length [ () | Major _ <- drop (length xs) steps ] 
            _ -> 0

runStrategyUntil :: [Int] -> Prefix -> a -> LabeledStrategy a -> [(a, Prefix)]
runStrategyUntil loc p a s = runGrammarUntilSt stopC False a $ maybe (withMarks s) snd $ runPrefix p s
 where
   stopC b (End is)  = (is==loc && b, b)
   stopC b (Major _) = (False, True)
   stopC b _         = (False, b)
   
firstMajorInPrefix :: Prefix -> Prefix -> LabeledStrategy a -> [Int]
firstMajorInPrefix p0@(P xs) p s = maybe [] (f 0 [[]]) (prefixToSteps (plusPrefix p0 p) s)
 where
   len = length xs
   f i stack@(hd:tl) (step:rest) = 
      case step of
         Major _  |  i >= len -> hd
         Begin is -> f (i+1) (is:stack) rest
         End _    -> f (i+1) tl rest
         _        -> f (i+1)stack rest
   f _ _ _ = []
   
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