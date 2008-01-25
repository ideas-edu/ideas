{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Common.Strategy 
   ( Strategy, NamedStrategy, IsStrategy(..), unlabel, label
   , (<*>), (<|>), (|>), succeed, failS, seqList, altList, repeatS, try, exhaustive, somewhere, somewhereTD
   , runStrategy, nextRule, nextRulesWith, nextRulesForSequenceWith, isSucceed, isFail, trackRule, trackRulesWith
   , intermediates, intermediatesList, check, traceStrategy, runStrategyRules, mapStrategy
   , StrategyLocation, strategyName
   , repeatNS, firstLocation, subStrategy, reportLocations
   ) where

import Common.Transformation
import Common.Move
import Common.Utils
import qualified Common.RegExp as RE
import Debug.Trace

-----------------------------------------------------------
--- Data type

-- | A strategy without any sub-strategies
newtype Strategy a = S { unS :: RE.RegExp (Either (Rule a) (NamedStrategy a)) }
 deriving Show

-- | A NamedStrategy is a strategy with sub-strategies and a top-level name.
data NamedStrategy a = Label { strategyName :: String, unlabel :: Strategy a }
 deriving Show

-----------------------------------------------------------
--- Strategy type classes

{- Apply -}
instance Apply Strategy where
   applyAll = runStrategy

instance Apply NamedStrategy where
   apply = apply . toStrategy

{- ToStrategy -}
class Apply f => IsStrategy f where
   toStrategy :: f a -> Strategy a
   
instance IsStrategy Rule where
   toStrategy = S . RE.symbol . Left

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy NamedStrategy where
   toStrategy = S . RE.symbol . Right

-----------------------------------------------------------
--- Smart constructors

label :: IsStrategy f => String -> f a -> NamedStrategy a
label l = Label l . toStrategy

noLabels :: Strategy a -> RE.RegExp (Rule a)
noLabels = RE.join . fmap (either RE.symbol (noLabels . unlabel)) . unS

infixr 5 <*>
infixr 4 |>, <|>

instance RE.IsRegExp Strategy where
   S p <*> S q = S $ p RE.<*> q
   S p <|> S q = S $ p RE.<|> q
   star (S p)  = S $ RE.star p
   succeed     = S $ RE.succeed
   emptyset    = S $ RE.emptyset

(<*>), (<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
p <*> q = toStrategy p RE.<*> toStrategy q
p <|> q	= toStrategy p RE.<|> toStrategy q

(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
p  |> q = p <|> (notS p <*> q)

succeed, failS :: Strategy a
succeed = S RE.succeed
failS   = S RE.emptyset

seqList, altList :: IsStrategy f => [f a] -> Strategy a
seqList = foldr ((RE.<*>) . toStrategy) RE.succeed
altList = foldr ((RE.<|>) . toStrategy) RE.emptyset

-- greedy!
repeatS :: IsStrategy f => f a -> Strategy a
repeatS p = repeatS_ng p <*> notS p

-- | solved
repeatNS :: IsStrategy f => f a -> Strategy a
repeatNS = repeatS

repeatS_ng :: IsStrategy f => f a -> Strategy a
repeatS_ng = RE.star . toStrategy

try :: IsStrategy f => f a -> Strategy a
try p = p |> succeed

check :: (a -> Bool) -> Strategy a
check p = toStrategy (minorRule $ makeSimpleRule "Check" $ \a -> if p a then Just a else Nothing)

notS :: Apply f => f a -> Strategy a
notS s = check (\a -> not $ applicable s a)

exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeatS . altList

-- | Poor man's solution: think harder about the Move type class, and currently, 
-- the strategy succeeds with at most 1 result, which is undesirable
somewhere :: (IsStrategy f, Move a) => f a -> Strategy a
somewhere p = ruleMoveTop <*> ruleMoveSomewhere <*> p <*> ruleMoveTop
 where
   ruleMoveSomewhere = minorRule $ makeSimpleRuleList "Somewhere" ${-  safeHead .  filter (applicable p) . -} reachable

-- top/down
somewhereTD :: (IsStrategy f, Move a) => f a -> Strategy a
somewhereTD p = somewhere p

-----------------------------------------------------------
--- Evaluation

runStrategy :: Strategy a -> a -> [a]
runStrategy strategy a =
   [ a | isSucceed strategy ] ++
   [ result | (rule, rest) <- firsts strategy, b <- applyAll rule a, result <- runStrategy rest b ]

traceStrategy :: Show a => Strategy a -> a -> [a]
traceStrategy strategy a = trace (show a) $ 
   [ trace (replicate 50 '-') a | isSucceed strategy ] ++
   [ result 
   | (rule, rest) <- firsts strategy
   , b <- applyAll rule a
   , result <- trace ("   ==> [" ++ show rule ++ "]") $ traceStrategy rest b 
   ]

isSucceed :: Strategy a -> Bool
isSucceed = RE.isSucceed . noLabels

isFail :: Strategy a -> Bool
isFail = RE.isEmptySet . noLabels

firsts :: Strategy a -> [(Rule a, Strategy a)]
firsts = concatMap f . RE.firsts . unS
 where
   f (Left r,   re) = [(r, S re)]
   f (Right ns, re) = [ (r, s <*> S re) | (r, s) <- firsts (unlabel ns) ] ++
                      if isSucceed (unlabel ns) then firsts (S re) else []


nextRule :: Strategy a -> a -> [(Rule a, a, Strategy a)]
nextRule strategy a = [ (r, b, s) | (r, s) <- firsts strategy, b <- applyAll r a ]

nextRulesWith :: (Rule a -> Bool) -> Strategy a -> a -> [([Rule a], a, Strategy a)]
nextRulesWith p strategy a = concatMap f (nextRule strategy a)
 where f (r, b, s) | p r       = [([r], b, s)]
                   | otherwise = [ (r:rs, c, t) | (rs, c, t) <- nextRulesWith p s b ] 

nextRulesForSequenceWith :: (Rule a -> Bool) -> Strategy a -> [a] -> [([Rule a], a)]
nextRulesForSequenceWith p strategy as = 
   case as of
      []     -> []
      [a]    -> let f (x, y, z) = (x, y)
                in map f (nextRulesWith p strategy a)
      a:rest -> [ (rs1++rs2, c)
                | (rs1, b, s) <- nextRulesWith p strategy a
                , (rs2, c)    <- nextRulesForSequenceWith p s rest 
                ]

trackRule :: Rule a -> Strategy a -> Strategy a
trackRule rule = 
   altList . map snd . filter ((==rule) . fst) . firsts

trackRulesWith :: (Rule a -> Bool) -> Rule a -> Strategy a -> a -> Strategy a
trackRulesWith p rule strategy = 
   altList . map thd3 . filter ((==rule) . last .  fst3) . nextRulesWith p strategy

intermediates :: Strategy a -> a -> [([Rule a], a, Strategy a)]
intermediates strategy = concat . intermediatesList strategy

-- list of results reflect the search depth
intermediatesList :: Strategy a -> a -> [[([Rule a], a, Strategy a)]]
intermediatesList strategy a = takeWhile (not . null) (iterate (concatMap next) start)
 where
   start = [([], a, strategy)]
   next (rs, a, s) = [ (r:rs, b, ns) | (r, b, ns) <- nextRule s a ]

-- variation of runStrategy: TODO, merge
runStrategyRules :: Strategy a -> a -> [([Rule a], a)]
runStrategyRules strategy a =
   [ ([], a) | isSucceed strategy ] ++
   [ (rule:rs, final) | (rule, rest) <- firsts strategy, b <- applyAll rule a, (rs, final) <- runStrategyRules rest b ]

-- returns a strategy without the Succeed alternative
nonSucceed :: Strategy a -> Strategy a
nonSucceed = S . RE.nonSucceed . unS

mapStrategy :: (Rule a -> Rule b) -> Strategy a -> Strategy b
mapStrategy f (S re) = S (fmap (either (Left . f) (Right . g)) re)
 where g (Label n s) = Label n (mapStrategy f s)

-----------------------------------------------------------
--- Substrategies

type StrategyLocation = [Int]

subStrategies :: NamedStrategy a -> [(StrategyLocation, Either (NamedStrategy a) (Rule a))]
subStrategies = rec [] 
 where
   rec loc ns = 
      let f is = either (\r -> [ (is, Right r) | not (isMinorRule r) ]) (rec is)
          xs   = RE.collectSymbols $ combine (,) loc $ unS $ unlabel ns
      in (loc, Left ns) : concatMap (uncurry f) xs

{- Domain.LinearAlgebra> Common.Strategy.reportLocations Domain.LinearAlgebra.toReducedEchelon -}
reportLocations :: NamedStrategy a -> IO ()
reportLocations = putStrLn . format2 . map f . subStrategies
 where
   f (loc, Left ns) = (g loc, strategyName ns)
   f (loc, Right r) = (g loc, name r ++ " (rule)")
   g loc = replicate (2*length loc) ' ' ++ show loc

format2 :: [(String, String)] -> String
format2 list = unlines $ map format list
 where
   wx     = width (map fst list)
   width  = maximum . map length
   make i = take i . (++repeat ' ')
   format (x, y) = make wx x ++ "   " ++ y

subStrategy :: StrategyLocation -> NamedStrategy a -> Maybe (NamedStrategy a)
subStrategy loc = fmap (either id f) . subStrategyOrRule loc
 where f r = label (name r) (toStrategy r)

subStrategyOrRule :: StrategyLocation -> NamedStrategy a -> Maybe (Either (NamedStrategy a) (Rule a))
subStrategyOrRule loc ns =
   case loc of
      [] -> return (Left ns)
      i:is -> do
         hd <- safeHead $ drop i $ RE.collectSymbols $ unS $ unlabel ns
         case hd of
            Left r
               | null is   -> return (Right r) 
               | otherwise -> Nothing
            Right ns -> 
               subStrategyOrRule is ns
 
withIndices :: NamedStrategy a -> RE.RegExp ([Int], Rule a)
withIndices = rec [] 
 where
   rec is = RE.join . combine f is . unS . unlabel
   f   is = either (RE.symbol . (,) is) (rec is)
      
firstLocation :: a -> NamedStrategy a -> Maybe StrategyLocation
firstLocation a ns = safeHead
   [ is | ((is, r), _) <- RE.firsts (withIndices ns), applicable r a ]
{-
nextLocation :: a -> NamedStrategy a -> StrategyLocation -> Maybe StrategyLocation
nextLocation a ns old = 
   let f new = take (g new + 1) new
       g = length . takeWhile id . zipWith (==) old
   in fmap f (firstLocation a ns)
   -}
-- local helper-function
combine :: ([Int] -> a -> b) -> [Int] -> RE.RegExp a -> RE.RegExp b
combine g is = fmap (\(i, a) -> g (is++[i]) a) . RE.withIndex