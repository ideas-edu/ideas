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
         
         case (applyAll subStrategy requestedTerm, maybe Nothing (fmap inContext . fromExpr) $ req_Answer req) of
            ([], _) -> replyError "strategy error" "not able to compute an expected answer"
            
            (answers, Just answeredTerm)
               | not (null witnesses) ->
                    Ok $ ReplyOk
                       { repOk_Strategy = req_Strategy req
                       , repOk_Location = loc
                       , repOK_Context  = showContext newContext
                       , repOk_Steps    = stepsRemaining (unlabel $ strategy a) (answeredTerm) -- not precise
                       }
                  where
                    witnesses = filter (equality a answeredTerm) answers
                    (loc, newContext) = nextLocation answeredTerm (req_Location req) a
                       
            (expected:_, maybeAnswer) ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = req_Location req ++ take 1 subloc
                      -- , repInc_Context    = showContext newContext
                       , repInc_Expected   = toExpr (fromContext expected)
                       , repInc_Steps      = stepsRemaining (unlabel $ strategy a) requestedTerm -- not precise
                       , repInc_Equivalent = maybe False (equivalence a expected) maybeAnswer
                       }
             where
               (subloc, newContext) = subTask requestedTerm subStrategy
                     
subTask :: a -> LabeledStrategy a -> (Location, a)
subTask term subStrategy = 
   fromMaybe ([], term) $ firstLocationWith (\_ -> not . isMinorRule) subStrategy term
      
nextLocation ::IsExpr a => Context a -> Location -> Assignment (Context a) -> (Location, Context a)
nextLocation term loc a = 
   case firstLocationWith (\p r -> not (loc `isPrefixOf` p) || not (isMinorRule r)) (strategy a) term of
      Just (is, a) -> (rec loc is, a)
      Nothing      -> (loc, term)
 where
   rec (i:is) (j:js)
      | i == j    = j : rec is js 
      | otherwise = [j]
   rec _ _        = [] 