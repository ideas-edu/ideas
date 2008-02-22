{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- A strategy is a context-free grammar with rules as symbols. Strategies can be 
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
   , fix, once, somewhere, topDown, bottomUp
     -- * Operations on strategies
   , runStrategy, acceptsEmpty
     -- * REST
   , nextRule, nextRulesWith, nextRulesForSequenceWith, trackRule, trackRulesWith
   , intermediates, intermediatesList, traceStrategy, runStrategyRules, mapStrategy, mapLabeledStrategy
   , StrategyLocation, remainingStrategy
   , firstLocation, firstLocationWith, subStrategy, reportLocations
   , emptyPrefix, continuePrefixUntil, runPrefix, Prefix(..)
   , withMarks, prefixToSteps, Step(..), runGrammarUntil, plusPrefix, runGrammarUntilSt
   ) where

import Prelude hiding (fail, not, repeat, sequence)
import qualified Prelude as Prelude
import Common.Transformation
import Common.Move
import Common.Utils
import qualified Common.Grammar as RE
import Debug.Trace

-----------------------------------------------------------
-- Data types and type classes

-- | Abstract data type for a strategy
newtype Strategy a = S { unS :: RE.Grammar (Either (Rule a) (LabeledStrategy a)) }

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

-- | A fix-point combinator on strategies (to model recursion). Powerful
-- (but dangerous) combinator
fix :: (Strategy a -> Strategy a) -> Strategy a
fix f = S $ RE.fix $ unS . f . S

-- | Apply a strategy on (exactly) one of the term's direct children
once :: (IsStrategy f, Move a) => f a -> Strategy a
once s = ruleMovesDown <*> s <*> ruleMoveUp

-- | Apply a strategy somewhere in the term
somewhere :: (IsStrategy f, Move a) => f a -> Strategy a
somewhere s = fix $ \this -> s <|> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- top-down approach
topDown :: (IsStrategy f, Move a) => f a -> Strategy a
topDown s = fix $ \this -> s |> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- bottom-up approach
bottomUp :: (IsStrategy f, Move a) => f a -> Strategy a
bottomUp s = fix $ \this -> once this <|> (not (once (bottomUp s)) <*> s)

{- The ideal implementation does not yet work: there appears to be a strange
   interplay between the fixpoint operator (with variables) and the not combinator
   > bottomUp s = fix $ \this -> once this |> s -}

-----------------------------------------------------------
--- Evaluation

runStrategy :: Strategy a -> a -> [a]
runStrategy strategy a =
   [ a | acceptsEmpty strategy ] ++
   [ result | (rule, rest) <- firsts strategy, b <- applyAll rule a, result <- runStrategy rest b ]

traceStrategy :: Show a => Strategy a -> a -> [a]
traceStrategy strategy a = trace (show a) $ 
   [ trace (replicate 50 '-') a | acceptsEmpty strategy ] ++
   [ result 
   | (rule, rest) <- firsts strategy
   , b <- applyAll rule a
   , result <- trace ("   ==> [" ++ show rule ++ "]") $ traceStrategy rest b 
   ]

-- local helper function
noLabels :: Strategy a -> RE.Grammar (Rule a)
noLabels = RE.join . fmap (either RE.symbol (noLabels . unlabel)) . unS

acceptsEmpty :: Strategy a -> Bool
acceptsEmpty = RE.acceptsEmpty . noLabels

firsts :: Strategy a -> [(Rule a, Strategy a)]
firsts = concatMap f . RE.firsts . unS
 where
   f (Left r,   re) = [(r, S re)]
   f (Right ns, re) = [ (r, s <*> S re) | (r, s) <- firsts (unlabel ns) ] ++
                      if acceptsEmpty (unlabel ns) then firsts (S re) else []


nextRule :: Strategy a -> a -> [(Rule a, a, Strategy a)]
nextRule strategy a = [ (r, b, s) | (r, s) <- firsts strategy, b <- applyAll r a ]

nextRulesWith :: (Rule a -> Bool) -> Strategy a -> a -> [([Rule a], a, Strategy a)]
nextRulesWith p strategy a = concatMap f (nextRule strategy a)
 where f (r, b, s) | p r       = [([r], b, s)]
                   | otherwise = [ (r:rs, c, t) | (rs, c, t) <- nextRulesWith p s b ] 

nextRulesForSequenceWith :: (a -> a -> Bool) -> (Rule a -> Bool) -> Strategy a -> [a] -> [([Rule a], a)]
nextRulesForSequenceWith eq p = rec
 where
   rec strategy as =
      case as of
         []     -> []
         [a]    -> let f (x, y, z) = (x, y)
                   in map f (nextRulesWith p strategy a)
         a:d:ds -> [ (rs1++rs2, c)
                   | (rs1, b, s) <- nextRulesWith p strategy a
                   , b `eq` d
                   , (rs2, c) <- rec s (b:ds)
                   ]

remainingStrategy :: (a -> a -> Bool) -> (Rule a -> Bool) -> Strategy a -> [a] -> Strategy a
remainingStrategy eq p s = alternatives . rec s
 where
   rec strategy as =
      case as of
         []     -> []
         [a]    -> [strategy]
         a:d:ds -> [ this
                   | (rs1, b, s) <- nextRulesWith p strategy a
                   , b `eq` d
                   , this <- rec s (b:ds)
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
   [ ([], a) | acceptsEmpty strategy ] ++
   [ (rule:rs, final) | (rule, rest) <- firsts strategy, b <- applyAll rule a, (rs, final) <- runStrategyRules rest b ]

-- returns a strategy without the Succeed alternative
nonSucceed :: Strategy a -> Strategy a
nonSucceed = S . RE.nonEmpty . unS

mapStrategy :: (Rule a -> Rule b) -> Strategy a -> Strategy b
mapStrategy f (S re) = S (fmap (either (Left . f) (Right . mapLabeledStrategy f)) re)

mapLabeledStrategy :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapLabeledStrategy f (Label n s) = Label n (mapStrategy f s)
 
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

reportLocations :: LabeledStrategy a -> [(StrategyLocation, String)]
reportLocations = map f . subStrategies
 where f (loc, e) = (loc, either forStrategy forRule e)
       forStrategy s = strategyName s ++ 
                       if all (either (const True) isMinorRule . snd) (subStrategies s) then " (skipped)" else ""
       forRule r = name r ++ " (rule)"

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
 
withIndices :: LabeledStrategy a -> RE.Grammar ([Int], Rule a)
withIndices = rec [] 
 where
   rec is = RE.join . combine f is . unS . tag is . unlabel
   f   is = either (RE.symbol . (,) is) (rec is)
   begin is = idRule {name="begin " ++ show is}
   end   is = idRule {name="end " ++ show is}
   tag is s = begin is <*> s <*> end is

data Step a = Minor [Int] (Rule a) | Major [Int] (Rule a) | Begin [Int] | End [Int]
   deriving Show 

withMarks :: LabeledStrategy a -> RE.Grammar (Step a)
withMarks = rec [] 
 where
   rec is = mark is . RE.join . combine f is . unS . unlabel
   f   is = either (RE.symbol . kind is) (rec is)
   kind is r = if isMinorRule r then Minor is r else Major is r
   mark is g = 
      let begin = RE.symbol (Begin is)
          end   = RE.symbol (End is) 
      in begin RE.<*> g RE.<*> end
   
firstLocation :: a -> LabeledStrategy a -> Maybe StrategyLocation
firstLocation a ns = safeHead
   [ is | ((is, r), _) <- RE.firsts (withIndices ns), applicable r a ]

firstLocationWith :: (StrategyLocation -> Rule a -> Bool) -> LabeledStrategy a -> a -> Maybe (StrategyLocation, a)
firstLocationWith p strategy = safeHead . rec (withIndices strategy)
 where
   rec gr a = 
      let f ((is, r), rest)
             | p is r    = [ (is, a) | applicable r a ]
             | otherwise = [ pair    | b <- applyAll r a, pair <- rec rest b ]
      in concatMap f (RE.firsts gr)
  
-- local helper-function
combine :: ([Int] -> a -> b) -> [Int] -> RE.Grammar a -> RE.Grammar b
combine g is = fmap (\(i, a) -> g (is++[i]) a) . RE.withIndex

-----------------------------------------------------------
--- Prefixes

newtype Prefix = P [Int] deriving Eq

instance Show Prefix where
   show (P is) = show is

emptyPrefix :: Prefix 
emptyPrefix = P []

singlePrefix :: Int -> Prefix
singlePrefix n = P [n]

plusPrefix :: Prefix -> Prefix -> Prefix
plusPrefix (P xs) (P ys) = P (xs++ys)

-- local helper function
runPrefix :: Prefix -> LabeledStrategy a -> Maybe ([Step a], RE.Grammar (Step a))
runPrefix (P xs) = rec [] xs . withMarks
 where
   rec zs [] g = return (reverse zs, g)
   rec zs (n:ns) g = 
      case drop n (RE.firsts g) of
         (z, h):_ -> rec (z:zs) ns h
         _        -> Nothing

runGrammar :: a -> RE.Grammar (Step a) -> [(a, Prefix)]
runGrammar = rec
 where
   rec a g
      | RE.acceptsEmpty g = [(a, emptyPrefix)]
      | otherwise         = concat (zipWith f [0..] (RE.firsts g))
    where
      add n xs = [ (a, P (n:ns)) | (a, P ns) <- xs ]
      f n (step, h) =
         case step of
            Begin _   -> add n $ rec a h 
            End _     -> add n $ rec a h
            Minor _ r -> [ (c, plusPrefix new p) | b <- applyAll r a, (c, p) <- rec b h ]
            Major _ r -> [ (b, new) | b <- applyAll r a ]
       where
         new = singlePrefix n
         
-- local helper function
runGrammarUntil :: (Step a -> Bool) -> a -> RE.Grammar (Step a) -> [(a, Prefix)]
runGrammarUntil stop = runGrammarUntilSt (\s step -> (stop step, s)) ()

runGrammarUntilSt :: (s -> Step a -> (Bool, s)) -> s -> a -> RE.Grammar (Step a) -> [(a, Prefix)]
runGrammarUntilSt stop s a g
   | RE.acceptsEmpty g = [(a, emptyPrefix)]
   | otherwise         = concat (zipWith f [0..] (RE.firsts g))
 where
   add n xs = [ (a, P (n:ns)) | (a, P ns) <- xs ]
   f n (step, h) =
      case step of
         Begin js  -> add n $ recStop a h 
         End _     -> add n $ recStop a h
         Minor _ r -> forRule n h r
         Major _ r -> forRule n h r
    where
      recStop a g
         | fst (stop s step) = [(a, emptyPrefix)]
         | otherwise = runGrammarUntilSt stop (snd $ stop s step) a g
      forRule n h r  = 
         [ (c, plusPrefix (singlePrefix n) p) | b <- applyAll r a, (c, p) <- recStop b h ]

prefixToSteps :: Prefix -> LabeledStrategy a -> Maybe [Step a]
prefixToSteps p = fmap fst . runPrefix p

continuePrefixUntil :: (Step a -> Bool) -> Prefix -> a -> LabeledStrategy a -> [(a, Prefix)]
continuePrefixUntil stop p a s = 
   case runPrefix p s of 
      Just (_, g) -> [ (b, plusPrefix p q) | (b, q) <- runGrammarUntil stop a g ]
      _           -> []