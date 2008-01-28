{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A strategy is a regular expression with rules as symbols. Strategies can be 
-- labeled with strings. A type class is introduced to lift all the combinators
-- that work on strategies, only to prevent that you have to insert these lifting
-- functions yourself.
--
-----------------------------------------------------------------------------
module Common.Strategy 
   ( -- * Data types and type classes
     Strategy, LabeledStrategy, strategyName, unlabel
   , IsStrategy(..)
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), succeed, fail, label, sequence, alternatives
     -- ** EBNF combinators
   , many, many1, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), exhaustive
     -- ** Traversal combinators
   , somewhere, topDown, bottomUp
     -- * REST
   , runStrategy, nextRule, nextRulesWith, nextRulesForSequenceWith, isSucceed, isFail, trackRule, trackRulesWith
   , intermediates, intermediatesList, traceStrategy, runStrategyRules, mapStrategy
   , StrategyLocation
   , firstLocation, subStrategy, reportLocations
   ) where

import Prelude hiding (fail, not, repeat, sequence)
import qualified Prelude as Prelude
import Common.Transformation
import Common.Move
import Common.Utils
import qualified Common.RegExp as RE
import Debug.Trace

-----------------------------------------------------------
-- Data types and type classes

-- | Abstract data type for a strategy
newtype Strategy a = S { unS :: RE.RegExp (Either (Rule a) (LabeledStrategy a)) }

-- | A strategy which is labeled with a string
data LabeledStrategy a = Label 
   { strategyName :: String  -- ^ Returns the label of the strategy
   , unlabel :: Strategy a   -- ^ Removes the label from a strategy
   }

-- | Type class to turn values into strategies
class Apply f => IsStrategy f where
   toStrategy :: f a -> Strategy a
   
-- instances for Show
instance Show a => Show (Strategy a) where
   show = show . unS

instance Show a => Show (LabeledStrategy a) where
   show (Label n s) = "label " ++ show n ++ " (" ++ show s ++ ")"

-- instances for Apply
instance Apply Strategy where
   applyAll = runStrategy

instance Apply LabeledStrategy where
   apply = apply . toStrategy

-- instances for IsStrategy
instance IsStrategy Rule where
   toStrategy = S . RE.symbol . Left

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy LabeledStrategy where
   toStrategy = S . RE.symbol . Right

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 5 <*>
infixr 4 <|>

-- | Put two strategies in sequence (first do this, then do that)
(<*>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <*> t = S (unS (toStrategy s) RE.<*> unS (toStrategy t))

-- | Choose between the two strategies (either do this or do that)
(<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <|> t	= S (unS (toStrategy s) RE.<|> unS (toStrategy t))

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = S RE.succeed

-- | The strategy that always fails
fail :: Strategy a
fail = S RE.fail

-- | Labels a strategy with a string
label :: IsStrategy f => String -> f a -> LabeledStrategy a
label l = Label l . toStrategy

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = foldr ((<*>) . toStrategy) succeed

-- | Combines a list of alternative strategies
alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = foldr ((<|>) . toStrategy) fail

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many = S . RE.star . unS . toStrategy

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 s = s <*> many s

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option s = s <|> succeed   

-- Negation and greedy combinators ----------------------

infixr 4 |>

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check p = toStrategy $ minorRule $ makeSimpleRule "check" $ \a ->
   if p a then Just a else Nothing

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: Apply f => f a -> Strategy a
not s = check (Prelude.not . applicable s)

-- | Repeat a strategy zero or more times (greedy version of 'many')
repeat :: IsStrategy f => f a -> Strategy a
repeat s = many s <*> not s

-- | Apply a certain strategy at least once (greedy version of 'many1')
repeat1 :: IsStrategy f => f a -> Strategy a
repeat1 s = many1 s <*> not s

-- | Apply a certain strategy if this is possible (greedy version of 'option')
try :: IsStrategy f => f a -> Strategy a
try s = s <|> not s

-- | Left-biased choice: if the left-operand strategy can be applied, do so. Otherwise,
--   try the right-operand strategy
(|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s |> t = s <|> (not s <*> t)

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeat . alternatives

-- Traversal combinators --------------------------------------------


-- | Poor man's solution: think harder about the Move type class, and currently, 
-- the strategy succeeds with at most 1 result, which is undesirable
somewhere :: (IsStrategy f, Move a) => f a -> Strategy a
somewhere p = ruleMoveTop <*> ruleMoveSomewhere <*> p <*> ruleMoveTop
 where
   ruleMoveSomewhere = minorRule $ makeSimpleRuleList "Somewhere" ${-  safeHead .  filter (applicable p) . -} reachable

topDown :: (IsStrategy f, Move a) => f a -> Strategy a
topDown p = somewhere p

bottomUp :: (IsStrategy f, Move a) => f a -> Strategy a
bottomUp p = somewhere p

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

-- local helper function
noLabels :: Strategy a -> RE.RegExp (Rule a)
noLabels = RE.join . fmap (either RE.symbol (noLabels . unlabel)) . unS

isSucceed :: Strategy a -> Bool
isSucceed = RE.isSucceed . noLabels

isFail :: Strategy a -> Bool
isFail = RE.isFail . noLabels

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
--                , (rs2, c)    <- if b==(head rest) then nextRulesForSequenceWith p s rest else (rs1, b) -- ugly should be [] of rs1
                , (rs2, c)    <- nextRulesForSequenceWith p s rest
                ]

trackRule :: Rule a -> Strategy a -> Strategy a
trackRule rule = 
   alternatives . map snd . filter ((==rule) . fst) . firsts

trackRulesWith :: (Rule a -> Bool) -> Rule a -> Strategy a -> a -> Strategy a
trackRulesWith p rule strategy = 
   alternatives . map thd3 . filter ((==rule) . last .  fst3) . nextRulesWith p strategy

intermediates :: Strategy a -> a -> [([Rule a], a, Strategy a)]
intermediates strategy = concat . intermediatesList strategy

-- list of results reflect the search depth
intermediatesList :: Strategy a -> a -> [[([Rule a], a, Strategy a)]]
intermediatesList strategy a = takeWhile (Prelude.not . null) (iterate (concatMap next) start)
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

subStrategies :: LabeledStrategy a -> [(StrategyLocation, Either (LabeledStrategy a) (Rule a))]
subStrategies = rec [] 
 where
   rec loc ns = 
      let f is = either (\r -> [ (is, Right r) | Prelude.not (isMinorRule r) ]) (rec is)
          xs   = RE.collectSymbols $ combine (,) loc $ unS $ unlabel ns
      in (loc, Left ns) : concatMap (uncurry f) xs

{- Domain.LinearAlgebra> Common.Strategy.reportLocations Domain.LinearAlgebra.toReducedEchelon -}
reportLocations :: LabeledStrategy a -> IO ()
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
   make i = take i . (++Prelude.repeat ' ')
   format (x, y) = make wx x ++ "   " ++ y

subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (LabeledStrategy a)
subStrategy loc = fmap (either id f) . subStrategyOrRule loc
 where f r = label (name r) (toStrategy r)

subStrategyOrRule :: StrategyLocation -> LabeledStrategy a -> Maybe (Either (LabeledStrategy a) (Rule a))
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
 
withIndices :: LabeledStrategy a -> RE.RegExp ([Int], Rule a)
withIndices = rec [] 
 where
   rec is = RE.join . combine f is . unS . unlabel
   f   is = either (RE.symbol . (,) is) (rec is)
      
firstLocation :: a -> LabeledStrategy a -> Maybe StrategyLocation
firstLocation a ns = safeHead
   [ is | ((is, r), _) <- RE.firsts (withIndices ns), applicable r a ]
{-
nextLocation :: a -> LabeledStrategy a -> StrategyLocation -> Maybe StrategyLocation
nextLocation a ns old = 
   let f new = take (g new + 1) new
       g = length . takeWhile id . zipWith (==) old
   in fmap f (firstLocation a ns)
   -}
-- local helper-function
combine :: ([Int] -> a -> b) -> [Int] -> RE.RegExp a -> RE.RegExp b
combine g is = fmap (\(i, a) -> g (is++[i]) a) . RE.withIndex