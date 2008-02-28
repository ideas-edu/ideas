module OpenMath.LAServer (respond, laServerFor, versionNr) where

import Domain.LinearAlgebra
import OpenMath.StrategyTable
import OpenMath.Request
import OpenMath.Reply
import OpenMath.ObjectParser
import Common.Apply
import Common.Context
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Exercise hiding (Pack, Incorrect, stepsRemaining)
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
   case [ ea | Entry _ ea@(Unpack a) _ _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [Unpack a] -> laServerFor a req
      _ -> replyError "request error" "unknown strategy"
   
laServerFor :: IsExpr a => Exercise (Context a) -> Request -> Reply
laServerFor a req = 
   case (subStrategyOrRule (req_Location req) (strategy a), getContextTerm req) of
   
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
                    witnesses   = filter (equality a answeredTerm . fst) answers
                    newPrefix   = getPrefix req `plusPrefix` snd (head witnesses)
                       
            ((expected,prefix):_, maybeAnswer) ->
                    Incorrect $ ReplyIncorrect
                       { repInc_Strategy   = req_Strategy req
                       , repInc_Location   = subTask (req_Location req) loc
                       , repInc_Expected   = toExpr (fromContext expected)
                                             -- only return arguments if we are at a rule
                       , repInc_Arguments  = if loc==req_Location req then args else Nothing
                       , repInc_Steps      = stepsRemaining (getPrefix req) requestedTerm (strategy a)
                       , repInc_Equivalent = maybe False (equivalence a expected) maybeAnswer
                       }
             where
               (loc, args) = firstMajorInPrefix (getPrefix req) prefix requestedTerm (strategy a)

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
            Just steps -> length [ () | Major _ _ <- drop (length xs) steps ] 
            _ -> 0

runStrategyUntil :: [Int] -> Prefix -> a -> LabeledStrategy a -> [(a, Prefix)]
runStrategyUntil loc p a s = runGrammarUntilSt stopC False a $ maybe (withMarks s) snd $ runPrefix p s
 where
   stopC b (End is)     = (is==loc && b, b)
   stopC b (Major is _) = (is==loc, True)
   stopC b (Minor is _) = (is==loc, b)
   stopC b _            = (False, b)
   
firstMajorInPrefix :: Prefix -> Prefix -> a -> LabeledStrategy a -> ([Int], Maybe String)
firstMajorInPrefix p0@(P xs) p a s = fromMaybe ([], Nothing) $ do
   steps <- prefixToSteps (plusPrefix p0 p) s
   let newSteps = drop (length xs) steps
   is    <- safeHead [ is | Major is _ <- newSteps ]
   return (is, argumentsForSteps a newSteps)
 
argumentsForSteps :: a -> [Step a] -> Maybe String
argumentsForSteps a = safeHead . flip rec a . stepsToRules
 where
   rec [] _ = []
   rec (r:rs) a
      | isMinorRule r  = concatMap (rec rs) (applyAll r a)
      | applicable r a = maybe [] return (arguments r a)
      | otherwise      = []
 
nextMajorForPrefix :: Prefix -> a -> LabeledStrategy a -> [Int]
nextMajorForPrefix p0@(P xs) a s = fromMaybe [] $ do
   (steps, g) <- runPrefix p0 s
   (_, p1)    <- safeHead (runGrammarUntil stopC a g)
   steps      <- prefixToSteps (plusPrefix p0 p1) s
   lastStep   <- safeHead (reverse steps)
   case lastStep of
      Major is _ -> return is
      _          -> Nothing
 where
   stopC (Major _ _) = True
   stopC _           = False
   
   
{-
   case lastStep of
      Major is r 
         | hasArguments r ->
              let rules = init $ drop (length xs) $ stepsToRules steps
                  term  = applyListD rules a
              in return (is, arguments r term)
         | otherwise -> return (is, Nothing)
      _              -> Nothing
-}