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
   ( Strategy, NamedStrategy, AnonymousStrategy, IsStrategy(..), unlabel, LiftStrategy(..)
   , (<*>), (<|>), (|>), succeed, failS, seqList, altList, repeatS, try, exhaustive, somewhere, somewhereTD
   , runStrategy, nextRule, nextRulesWith, isSucceed, isFail, trackRule, trackRulesWith
   , intermediates, intermediatesList, check, traceStrategy, runStrategyRules, mapStrategy
   , StrategyLocation, strategyName, subStrategies, subStrategy, subStrategyOrRule
   , repeatNS, reportLocations, firstLocation
   ) where

import Common.Transformation
import Common.Move
import Common.Utils
import qualified Common.RegExp as RE
import Test.QuickCheck hiding (check, label)
import Control.Monad
import Control.Arrow
import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace

-----------------------------------------------------------
--- Data type

-- | A strategy without any sub-strategies
newtype Strategy a = S { unS :: RE.RegExp (Rule a) }
 deriving Show

-- | An AnonymousStrategy is a strategy with sub-strategies, but without 
-- | a top-level name. Use "label" to turn an AnonymousStrategy into a
-- | NamedStrategy.
newtype AnonymousStrategy a = AS { unAS :: RE.RegExp (NamedStrategy a) }
 deriving Show

-- | A NamedStrategy is a strategy with sub-strategies and a top-level name.
newtype NamedStrategy a = NS (String, Either (AnonymousStrategy a) (Strategy a))
 deriving Show
 
-----------------------------------------------------------
--- Strategy type classes

{- Apply -}
instance Apply Strategy where
   applyAll = runStrategy

instance Apply NamedStrategy where
   apply = apply . toStrategy

instance Apply AnonymousStrategy where 
   apply = apply . toStrategy

{- ToStrategy -}
class Apply f => IsStrategy f where
   toStrategy :: f a -> Strategy a
   label      :: String -> f a -> NamedStrategy a
   
instance IsStrategy Rule where
   toStrategy = S . RE.symbol
   label l    = label l . toStrategy

instance IsStrategy Strategy where
   toStrategy = id
   label l s  = NS (l, Right s)
   
instance IsStrategy AnonymousStrategy where
   toStrategy (AS f) = S $ RE.join $ fmap (unS . unlabel) f
   label l as        = NS (l, Left as)
   
instance IsStrategy NamedStrategy where
   toStrategy (NS (_, f)) = either toStrategy toStrategy f
   label l = label l . AS . RE.symbol

{- LiftStrategy -}
class (IsStrategy f, IsStrategy g, RE.IsRegExp g) => LiftStrategy f g | f -> g where
   liftStrategy :: f a -> g a

instance LiftStrategy Rule Strategy where
  liftStrategy = toStrategy
  
instance LiftStrategy Strategy Strategy where
  liftStrategy = id

instance LiftStrategy NamedStrategy AnonymousStrategy where
  liftStrategy = AS . RE.symbol

instance LiftStrategy AnonymousStrategy AnonymousStrategy where
  liftStrategy = id

-----------------------------------------------------------
--- Smart constructors

unlabel :: NamedStrategy a -> Strategy a
unlabel = toStrategy 

infixr 5 <*>
infixr 4 |>, <|>

instance RE.IsRegExp Strategy where
   S p <*> S q = S $ p RE.<*> q
   S p <|> S q = S $ p RE.<|> q
   star (S p)  = S $ RE.star p
   succeed     = S $ RE.succeed
   emptyset    = S $ RE.emptyset

instance RE.IsRegExp AnonymousStrategy where
   AS p <*> AS q = AS $ p RE.<*> q
   AS p <|> AS q = AS $ p RE.<|> q
   star (AS p)   = AS $ RE.star p
   succeed       = AS $ RE.succeed
   emptyset      = AS $ RE.emptyset

(<*>), (<|>) :: (LiftStrategy f s, LiftStrategy g s) => f a -> g a -> s a
p <*> q = liftStrategy p RE.<*> liftStrategy q
p <|> q	= liftStrategy p RE.<|> liftStrategy q
  
(|>) :: (LiftStrategy f Strategy, LiftStrategy g Strategy) => f a -> g a -> Strategy a
p  |> q = p <|> (notS p <*> q)

succeed, failS :: Strategy a
succeed = S RE.succeed
failS   = S RE.emptyset

seqList, altList :: LiftStrategy f s => [f a] -> s a
seqList = foldr ((RE.<*>) . liftStrategy) RE.succeed
altList = foldr ((RE.<|>) . liftStrategy) RE.emptyset

-- greedy!
repeatS :: LiftStrategy f Strategy => f a -> Strategy a
repeatS p = repeatS_ng p <*> notS p

-- | Poor man's solution: how to negate a named strategy?
repeatNS :: AnonymousStrategy a -> AnonymousStrategy a
repeatNS s = repeatS_ng s <*> label "_repeatNS" (notS s)

repeatS_ng :: LiftStrategy f s => f a -> s a
repeatS_ng = RE.star . liftStrategy

try :: LiftStrategy f Strategy => f a -> Strategy a
try p = p |> succeed

check :: (a -> Bool) -> Strategy a
check p = toStrategy (minorRule $ makeSimpleRule "Check" $ \a -> if p a then Just a else Nothing)

notS :: Apply f => f a -> Strategy a
notS s = check (\a -> not $ applicable s a)

exhaustive :: LiftStrategy f Strategy => [f a] -> Strategy a
exhaustive = repeatS . altList

-- | Poor man's solution: think harder about the Move type class, and currently, 
-- the strategy succeeds with at most 1 result, which is undesirable
somewhere :: (LiftStrategy f Strategy, Move a) => f a -> Strategy a
somewhere p = ruleMoveTop <*> ruleMoveSomewhere <*> p <*> ruleMoveTop
 where
   ruleMoveSomewhere = minorRule $ makeSimpleRuleList "Somewhere" ${-  safeHead . filter (applicable p) -}  reachable

-- top/down
somewhereTD :: (LiftStrategy f Strategy, Move a) => f a -> Strategy a
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
isSucceed = RE.isSucceed . unS

isFail :: Strategy a -> Bool
isFail (S p) = RE.isEmptySet p

firsts :: Strategy a -> [(Rule a, Strategy a)]
firsts = map (second S) . RE.firsts . unS

nextRule :: Strategy a -> a -> [(Rule a, a, Strategy a)]
nextRule strategy a = [ (r, b, s) | (r, s) <- firsts strategy, b <- applyAll r a ]

nextRulesWith :: (Rule a -> Bool) -> Strategy a -> a -> [([Rule a], a, Strategy a)]
nextRulesWith p strategy a = concatMap f (nextRule strategy a)
 where f (r, b, s) | p r       = [([r], b, s)]
                   | otherwise = [ (r:rs, c, t) | (rs, c, t) <- nextRulesWith p s b ] 

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
mapStrategy f (S regexp) = S (fmap f regexp)

applyS :: (a -> b) -> (b -> Strategy a) -> Strategy a
applyS f g = undefined

-----------------------------------------------------------
--- Substrategies

type StrategyLocation = [Int]

strategyName :: NamedStrategy a -> String
strategyName (NS (name, _)) = name

subStrategies :: NamedStrategy a -> [(StrategyLocation, Either (NamedStrategy a) (Rule a))]
subStrategies = rec [] 
 where
   rec loc ns@(NS (_, s)) = (loc, Left ns) : 
      case s of
         Left (AS re) -> concat $ RE.collectSymbols $ combine rec loc re
         Right (S re) -> zipWith (\i r -> (loc++[i], Right r)) [0..] (filter (not . isMinorRule) $ RE.collectSymbols re)

{- Domain.LinearAlgebra> Common.Strategy.reportLocations Domain.LinearAlgebra.toReducedEchelon -}
reportLocations :: NamedStrategy a -> IO ()
reportLocations = putStrLn . format2 . map f . subStrategies
 where
   f (loc, Left (NS (s, _))) = (g loc, s)
   f (loc, Right r)          = (g loc, name r ++ " (rule)")
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
subStrategyOrRule [] ns = Just (Left ns)
subStrategyOrRule (i:is) (NS (_, s)) = 
   case s of
      Left (AS re) -> 
         case drop i (RE.collectSymbols re) of
            hd:_ -> subStrategyOrRule is hd
            _    -> Nothing
      Right (S re) -> 
         case drop i (RE.collectSymbols re) of 
            hd:_ | null is -> Just (Right hd)
            _    -> Nothing

withIndices :: NamedStrategy a -> RE.RegExp ([Int], Rule a)
withIndices = rec [] 
 where
   rec is (NS (_, s)) = 
      case s of
         Left (AS re) -> RE.join (combine rec is re)
         Right (S re) -> combine (,) is re
            
firstLocation :: a -> NamedStrategy a -> Maybe StrategyLocation
firstLocation a ns = safeHead
   [ is | ((is, r), _) <- RE.firsts (withIndices ns), applicable r a ]

nextLocation :: a -> NamedStrategy a -> StrategyLocation -> Maybe StrategyLocation
nextLocation a ns old = 
   let f new = take (g new + 1) new
       g = length . takeWhile id . zipWith (==) old
   in fmap f (firstLocation a ns)
   
-- local helper-function
combine :: ([Int] -> a -> b) -> [Int] -> RE.RegExp a -> RE.RegExp b
combine g is = fmap (\(i, a) -> g (is++[i]) a) . RE.withIndex