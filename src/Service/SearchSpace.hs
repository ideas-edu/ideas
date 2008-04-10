module Service.SearchSpace where

import Common.Apply
import Common.Context
import Common.Strategy (somewhere, Prefix, emptyPrefix, runPrefixMajor, lastRuleInPrefix)
import Common.Transformation
import Service.Progress
import qualified Data.Set as S

import Domain.Logic

costSTRATEGY, costRULE, costEXPENSIVE, costBUGGY :: Integer
costSTRATEGY  = 1
costRULE      = 2
costEXPENSIVE = 10
costBUGGY     = 15

stepP :: (Uniplate a, Ord score, Num score) => [(score, Rule (Context a))] -> Context a -> Progress score (Context a, Rule (Context a))
stepP rules a = do
   r <- scoreList rules
   let ps = applyAll (somewhere r) a
   fromList $ map (\x -> (x, r)) ps
   
searchSpace :: (Ord a, Uniplate a, Ord score, Num score) => Prefix (Context a) -> [(score, Rule (Context a))] -> Context a -> Progress score (Context a, [Rule (Context a)])
searchSpace p0 rules q = rec S.empty (success (q, [], Just p0))
 where
   rec history worklist =
      case extractFirst worklist of
         Nothing -> 
            emptyProgress
         Just (cost, (p, rs, mStrat), _, rest)
            | S.member p history ->
                 addScore cost failure <||> rec history rest
            | otherwise -> 
                 let new =  mapProgress (\(a, r) -> (a, r:rs, Nothing)) (stepP rules p)
                        <|> addScore (fromInteger costSTRATEGY) (
                               case mStrat of
                                  Nothing -> emptyProgress
                                  Just p1 -> fromList 
                                     [ (x, r:rs, Just y) 
                                     | (x, y) <- runPrefixMajor p1 p
                                     , Just r <- [lastRuleInPrefix y], isMajorRule r
                                     ])
                     newHistory  = S.insert p history
                     newWorklist = addScore cost new <|> rest
                 in addScore cost (success (p, rs)) <||> rec newHistory newWorklist

instance Ord a => Ord (Context a) where
   x `compare` y = fromContext x `compare` fromContext y
   
   
------------------------------------------------------
-- Example for logic domain

buggyRules     = map liftRuleToContext [buggyDeMorganOr, buggyDeMorganAnd, buggyAndOverOr, buggyOrOverAnd]
expensiveRules = map liftRuleToContext [ruleDefEquiv, ruleAndOverOr, ruleOrOverAnd]

buggyDeMorganOr :: LogicRule
buggyDeMorganOr = buggyRule $ makeRule "Buggy DeMorganOr" $
   (Not (x :||: y))  |-  (Not x :||: Not y)

buggyDeMorganAnd :: LogicRule
buggyDeMorganAnd = buggyRule $ makeRule "Buggy DeMorganAnd" $
   (Not (x :&&: y))  |-  (Not x :&&: Not y)
   
buggyAndOverOr :: LogicRule
buggyAndOverOr = buggyRule $ makeRuleList "Buggy AndOverOr"
   [ (x :&&: (y :||: z))  |-  ((x :||: y) :&&: (x :||: z))
   , ((x :||: y) :&&: z)  |-  ((x :||: z) :&&: (y :||: z))
   ]

buggyOrOverAnd :: LogicRule
buggyOrOverAnd = buggyRule $ makeRuleList "Buggy OrOverAnd"
   [ (x :||: (y :&&: z))  |-  ((x :&&: y) :||: (x :&&: z))
   , ((x :&&: y) :||: z)  |-  ((x :&&: z) :||: (y :&&: z))
   ]

ex = (T :->: Var "p") :&&: (Var "q" :||: T)

q = putStrLn $ unlines $ map f $ successes $ maxDepth 20 $ stepsP (inContext ex)
 where f (a, rs) = ppLogic (fromContext a) ++ "  " ++ show rs

w = map (\(a,b) -> (a, length b)) $ successesForScore $ maxDepth 25 $ stepsP (inContext ex)

ruleScore :: Rule (Context Logic) -> Int
ruleScore r
   | name r `elem` map name buggyRules     = fromInteger costBUGGY
   | name r `elem` map name expensiveRules = fromInteger costEXPENSIVE
   | otherwise                             = fromInteger costRULE

stepsP :: Context Logic -> Progress Int (Context Logic, [Rule (Context Logic)])
stepsP = searchSpace (emptyPrefix toDNF) rules
 where
   list  = map liftRuleToContext logicRules ++ buggyRules
   rules = zip (map ruleScore list) list