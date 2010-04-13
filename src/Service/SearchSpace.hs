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
module Service.SearchSpace 
   ( searchSpace, searchSpaceWith, SearchSpaceConfig(..), defaultConfig
   ) where

import Common.Apply
import Common.Rewriting.TreeDiff
import Common.Transformation
import Common.Strategy hiding ((<||>), (<|>))
import Service.Progress
import qualified Data.Set as S

data SearchSpaceConfig = SSC 
   { costStrategy :: Rational
   , costRule     :: String -> Rational
   }
   
defaultConfig :: SearchSpaceConfig
defaultConfig = SSC 
   { costStrategy = 1
   , costRule     = const 1
   }

type Diffs a = a -> [(a, TreeDiff)] 

searchSpace :: (a -> a -> Ordering) -> Diffs a -> Maybe (Prefix a) -> [Rule a] -> a -> Progress Rational (a, Maybe (Prefix a), [Rule a])
searchSpace = searchSpaceWith defaultConfig

searchSpaceWith :: SearchSpaceConfig -> (a -> a -> Ordering) -> Diffs a -> Maybe (Prefix a) -> [Rule a] -> a -> Progress Rational (a, Maybe (Prefix a), [Rule a])
searchSpaceWith config ordering diffs mp rules q = rec (empty ordering) (success (q, [], mp))
 where
   rec history worklist =
      case extractFirst worklist of
         Nothing -> 
            emptyProgress
         Just (cost, (p, rs, mStrat), _, rest)
            | member p history ->
                 addScore cost failure <||> rec history rest
            | otherwise -> 
                 let new = newStrategy <|> newRule
                     newRule = mapProgress (\(a, r) -> (a, r:rs, Nothing)) (stepP p)
                     newStrategy = 
                        case mStrat of
                           Nothing -> emptyProgress
                           Just p1 -> addScore (costStrategy config) $ fromMaybeList $
                              flip map (runPrefixMajor p1 p) $ \(x, y) -> 
                                 case lastRuleInPrefix y of
                                    Just r | isMajorRule r && stepsToRules (prefixToSteps p1) /= stepsToRules (prefixToSteps y) -> 
                                       Just (x, r:rs, Just y)
                                    _ -> Nothing
                     newHistory  = insert p history
                     newWorklist = addScore cost new <|> rest
                 in addScore cost (success (p, mStrat, rs)) <||> rec newHistory newWorklist

   stepP a0 = do
      (r, a) <- scoreList 
         [ (cost , (r, a)) 
         | (a, td) <- diffs a0
         , r <- rules 
         , let cost = costRule config (name r) * scoreTreeDiff td
         ]
      fromMaybeList $ 
         case applyAll r a of
            [] -> [ Nothing ]
            bs -> [ Just (b, r) | b <- bs]

scoreTreeDiff :: TreeDiff -> Rational
scoreTreeDiff td =
   case td of
      Equal -> 1000 
      _ -> 1
      {- Equal     -> 20
      Inside    -> 10
      Different -> 1
      Top       -> 2 -}

-- History and X are a work-around, since we don't have an Ord instance for our type
data History a = History (a -> a -> Ordering) (S.Set (X a))

newtype X a = X (a -> a -> Ordering, a)

instance Eq (X a) where
   X (f, a) == X (_, b) = f a b == EQ

instance Ord (X a) where
   X (f, a) `compare` X (_, b) = f a b

empty :: (a -> a -> Ordering) -> History a 
empty f = History f S.empty

member :: a -> History a -> Bool
member a (History f s) = S.member (X (f, a)) s

insert :: a -> History a -> History a
insert a (History f s) = History f (S.insert (X (f, a)) s)

------------------------------------------------------
-- Example for logic domain
{- 
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
   rules = zip (map ruleScore list) list -}