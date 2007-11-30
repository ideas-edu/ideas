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
   ( Strategy, IsStrategy(..)
   , (<*>), (<|>), (|>), succeed, failS, seqList, altList, repeatS, try, exhaustive, somewhere, somewhereTD
   , runStrategy, nextRule, nextRulesWith, isSucceed, isFail, trackRule, trackRulesWith
   , intermediates, intermediatesList, check
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

-----------------------------------------------------------
--- Data type

newtype Strategy a = S { unS :: RE.RegExp (Rule a) }
 deriving Show

data NamedStrategy a = NS String (Either (RE.RegExp (NamedStrategy a)) (Strategy a))

instance Apply NamedStrategy where
   apply = apply . unlabelStrategy

instance IsStrategy NamedStrategy where
   toStrategy = unlabelStrategy

unlabelStrategy :: NamedStrategy a -> Strategy a
unlabelStrategy (NS _ s) = either (S . RE.join . fmap (unS . toStrategy)) id s
 
labelStrategy :: String -> Strategy a -> NamedStrategy a
labelStrategy x = NS x . Right

-----------------------------------------------------------
--- Strategy type class

class Apply s => IsStrategy s where
   toStrategy :: s a -> Strategy a

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy Rule where
   toStrategy = S . RE.Step

-----------------------------------------------------------
--- Smart constructors

infixr 5 <*>
infixr 4 |>, <|>
 
(<*>), (<|>), (|>) :: (IsStrategy s, IsStrategy t) => s a -> t a -> Strategy a
p <*> q = S $ unS (toStrategy p) RE.<*> unS (toStrategy q)
p <|> q = S $ unS (toStrategy p) RE.<|> unS (toStrategy q)
p  |> q = p <|> (notS p <*> q)

succeed, failS :: Strategy a
succeed = S RE.Succeed
failS   = S RE.Fail

seqList, altList :: IsStrategy s => [s a] -> Strategy a
seqList = foldr (<*>) succeed
altList = foldr (<|>) failS

-- greedy!
repeatS :: IsStrategy s => s a -> Strategy a
repeatS p = repeatS_ng p <*> notS p

repeatS_ng :: IsStrategy s => s a -> Strategy a
repeatS_ng = S . RE.star . unS . toStrategy

try :: IsStrategy s => s a -> Strategy a
try p = p |> succeed

check :: (a -> Bool) -> Strategy a
check p = toStrategy (minorRule $ makeSimpleRule "Check" $ \a -> if p a then Just a else Nothing)

notS :: IsStrategy s => s a -> Strategy a
notS s = check (\a -> not $ applicable (toStrategy s) a)

exhaustive :: IsStrategy s => [s a] -> Strategy a
exhaustive = repeatS . altList . map toStrategy

-- | Poor man's solution: think harder about the Move type class, and currently, 
-- the strategy succeeds with at most 1 result, which is undesirable
somewhere :: (IsStrategy s, Move a) => s a -> Strategy a
somewhere p = ruleMoveTop <*> ruleMoveSomewhere <*> p <*> ruleMoveTop
 where
   ruleMoveSomewhere = minorRule $ makeSimpleRule "Somewhere" $ safeHead . filter (applicable p) . reachable

-- top/down
somewhereTD :: (IsStrategy s, Move a) => s a -> Strategy a
somewhereTD p = somewhere p

-----------------------------------------------------------
--- Evaluation

runStrategy :: Strategy a -> a -> [a]
runStrategy strategy a =
   [ a | isSucceed strategy ] ++
   [ result | (rule, rest) <- firsts strategy, b <- applyM rule a, result <- runStrategy rest b ]

instance Apply Strategy where
   apply strategy = safeHead . runStrategy strategy
         
isSucceed :: Strategy a -> Bool
isSucceed = RE.isSucceed . unS

isFail :: Strategy a -> Bool
isFail (S RE.Fail) = True
isFail _           = False

firsts :: Strategy a -> [(Rule a, Strategy a)]
firsts = map (second S) . RE.firsts . unS

nextRule :: Strategy a -> a -> [(Rule a, a, Strategy a)]
nextRule strategy a = [ (r, b, s) | (r, s) <- firsts strategy, b <- applyM r a ]

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

-- returns a strategy without the Succeed alternative
nonSucceed :: Strategy a -> Strategy a
nonSucceed = S . RE.nonSucceed . unS