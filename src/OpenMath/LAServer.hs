module OpenMath.LAServer (respond) where

import Domain.LinearAlgebra
import Domain.LinearAlgebra.Checks (reduceMatrixAssignment)
import OpenMath.StrategyTable
import OpenMath.Request
import OpenMath.Reply
import OpenMath.ObjectParser
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Assignment hiding (Incorrect)
import Data.Maybe
import Data.Char

respond :: Maybe String -> String
respond = replyInXML . maybe requestError (either parseError laServer . pRequest)

replyError :: String -> String -> Reply
replyError kind = Error . ReplyError kind

parseError :: String -> Reply
parseError   = replyError "parse error"

requestError :: Reply
requestError = replyError "request error" "no request found in \"input\""

(~=) :: String -> String -> Bool
xs ~= ys = let f = map toLower . filter (not . isSpace)
           in f xs == f ys 

laServer :: Request -> Reply
laServer req | not (req_Strategy req ~= "Gaussian elimination") =  -- Quick and dirty check!
   replyError "request error" "unknown strategy"
laServer req =
   case subStrategy (req_Location req) toReducedEchelon of
      Nothing -> 
         replyError "location error" "invalid location for strategy"
      Just subStrategy -> 
         case applyAll subStrategy (inContext $ fromJust $ fromExpr $ req_Term req) of
            [] -> replyError "strategy error" "not able to compute an expected answer"
            answers@(expected:_)
               | fmap (inContext . fromJust . fromExpr) (req_Answer req) `elem` (map Just answers) ->
                    Ok $ ReplyOk
                       { repOk_Strategy = req_Strategy req
                       , repOk_Location = nextLocation (fromJust $ fromExpr $ fromJust $ req_Answer req) (req_Location req)
                       , repOk_Steps    = stepsRemaining reduceMatrixAssignment (inContext $ fromJust $ fromExpr $ fromJust $ req_Answer req)
                       } 
               | otherwise ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = req_Location req ++ subTask (fromJust $ fromExpr $ req_Term req) subStrategy
                       , repInc_Expected   = toExpr $ matrix expected
                       , repInc_Steps      = stepsRemaining reduceMatrixAssignment (inContext $ fromJust $ fromExpr $ req_Term req)
                       , repInc_Equivalent = case req_Answer req of
                                                Nothing -> False
                                                Just m  -> equivalence reduceMatrixAssignment expected (inContext $ fromJust $ fromExpr m)
                       }
                       
subTask :: Matrix Rational -> LabeledStrategy (MatrixInContext Rational) -> Location
subTask term subStrategy = 
   case firstLocation (inContext term) subStrategy of
      Just (i:_) -> [i] -- one-level only
      _          -> []
      
nextLocation :: Matrix Rational -> Location -> Location
nextLocation term loc = 
   case firstLocation (inContext term) toReducedEchelon of
      Just new -> rec loc new
      Nothing  -> loc
 where
   rec (i:is) (j:js)
      | i == j    = j : rec is js 
      | otherwise = [j]
   rec _ js       = take 1 js