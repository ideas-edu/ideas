-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
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
     -- * Running strategies
   , derivationTree -- , runWithSteps, traceStrategy
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), (<||>), succeed, fail, label, sequence, alternatives
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), exhaustive
     -- ** Traversal combinators
   , fix, once, somewhere, topDown, bottomUp
     -- * Strategy locations
   , StrategyLocation, StrategyOrRule, strategyLocations, subStrategy
   , mapRules, rulesInStrategy, cleanUpStrategy
     -- * Prefixes
   , Prefix, emptyPrefix, makePrefix
   , Step(..), runPrefix, runPrefixUntil, runPrefixMajor, runPrefixLocation  
   , prefixToSteps, stepsToRules, lastRuleInPrefix
   ) where

import Common.Apply
import Common.Context
import Common.Derivation
import Common.Rewriting hiding (inverse)
import Common.Transformation
import Common.Uniplate (Uniplate, children)
import Common.Utils
import Prelude hiding (fail, not, repeat, replicate, sequence)
import qualified Common.Grammar as RE
import qualified Prelude as Prelude

-----------------------------------------------------------
-- Data types and type classes

-- | Abstract data type for a strategy
newtype Strategy a = S { unS :: RE.Grammar (Either (Rule a) (LabeledStrategy a)) }

-- | A strategy which is labeled with a string
data LabeledStrategy a = LS 
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
   show s = 
      strategyName s ++ ": " ++ show (unlabel s)

-- instances for Apply
instance Apply Strategy where
   applyAll s = results . derivationTree s

instance Apply LabeledStrategy where
   applyAll = applyAll . unlabel

-- instances for IsStrategy
instance IsStrategy RewriteRule where
   toStrategy r = 
      toStrategy (makeRule (ruleName r) (RewriteRule r))

instance IsStrategy Rule where
   toStrategy = S . RE.symbol . Left

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy (LabeledStrategy) where
  toStrategy = S . RE.symbol . Right

-- instances for Lift
instance Lift Strategy where
   lift lp (S re) = S (fmap (either (Left . lift lp) (Right . lift lp)) re)
   
instance Lift (LabeledStrategy) where
   lift lp (LS n s) = LS n (lift lp s)

-----------------------------------------------------------
--- Running strategies

-- | Returns the derivation tree for a strategy and a term
derivationTree :: IsStrategy f => f a -> a -> DerivationTree (Rule a) a
derivationTree = rec . noLabels . toStrategy
 where
   rec s a  = addBranches (list s a) (singleNode a (RE.empty s))
   list s a = [ (f, rec rest b)
              | (f, rest) <- RE.firsts s
              , b <- applyAll f a 
              ]

-- local helper function
noLabels :: Strategy a -> RE.Grammar (Rule a)
noLabels = RE.join . fmap (either RE.symbol (noLabels . unlabel)) . unS

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 3 <|>
infixr 4 <||>
infixr 5 <*>

-- | Put two strategies in sequence (first do this, then do that)
(<*>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <*> t = S (unS (toStrategy s) RE.<*> unS (toStrategy t))

-- | Choose between the two strategies (either do this or do that)
(<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <|> t = S (unS (toStrategy s) RE.<|> unS (toStrategy t))

-- | Run two strategies in parallel (with interleaving)
(<||>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <||> t = S (unS (toStrategy s) RE.<||> unS (toStrategy t))

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = S RE.succeed

-- | The strategy that always fails
fail :: Strategy a
fail = S RE.fail

-- | Labels a strategy with a string
label :: IsStrategy f => String -> f a -> LabeledStrategy a
label l = LS l . toStrategy

-- | Puts a list of strategies into a sequence
sequence :: IsStrategy f => [f a] -> Strategy a
sequence = foldr ((<*>) . toStrategy) succeed

-- | Combines a list of alternative strategies
alternatives :: IsStrategy f => [f a] -> Strategy a
alternatives = foldr ((<|>) . toStrategy) fail

-- EBNF combinators --------------------------------------

-- | Repeat a strategy zero or more times (non-greedy)
many :: IsStrategy f => f a -> Strategy a
many = S . RE.many . unS . toStrategy

-- | Apply a certain strategy at least once (non-greedy)
many1 :: IsStrategy f => f a -> Strategy a
many1 s = s <*> many s

-- | Apply a strategy a certain number of times
replicate :: IsStrategy f => Int -> f a -> Strategy a
replicate n = sequence . Prelude.replicate n

-- | Apply a certain strategy or do nothing (non-greedy)
option :: IsStrategy f => f a -> Strategy a
option s = s <|> succeed   

-- Negation and greedy combinators ----------------------

infixr 4 |>

-- | Checks whether a predicate holds for the current term. The
--   check is considered to be a minor step.
check :: (a -> Bool) -> Strategy a
check p = toStrategy checkRule 
 where
   checkRule = minorRule $ hasInverse checkRule $ makeSimpleRule "check" $ \a ->
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
once :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
once s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ hasInverse ruleMoveUp $ makeSimpleRuleList "MoveDown" moveDown
   moveDown c = 
      let n = maybe 0 (pred . length . children) (currentFocus c)
      in [ changeLocation (locationDown i) c | i <- [0 .. n] ]
   
   ruleMoveUp = minorRule $ hasInverse ruleMoveDown $ makeSimpleRule "MoveUp" moveUp
   moveUp c   = do
      new <- locationUp (location c)
      return $ setLocation new c

-- | Apply a strategy somewhere in the term
somewhere :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
somewhere s = fix $ \this -> s <|> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- top-down approach
topDown :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
topDown s = fix $ \this -> s |> once this

-- | Search for a suitable location in the term to apply the strategy using a
-- bottom-up approach
bottomUp :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
bottomUp s = fix $ \this -> once this <|> (not (once (bottomUp s)) <*> s)

{- The ideal implementation does not yet work: there appears to be a strange
   interplay between the fixpoint operator (with variables) and the not combinator
   > bottomUp s = fix $ \this -> once this |> s -}
                      
-----------------------------------------------------------
--- Strategy locations

-- | A strategy location corresponds to a substrategy or a rule
type StrategyLocation = [Int]

type StrategyOrRule a = Either (LabeledStrategy a) (Rule a)

-- | Returns a list of all strategy locations, paired with the labeled substrategy or rule at that location
strategyLocations :: LabeledStrategy a -> [(StrategyLocation, Either (LabeledStrategy a) (Rule a))]
strategyLocations = rec [] 
 where
   rec loc ns = 
      let f is = either (\r -> [ (is, Right r) | isMajorRule r ]) (rec is)
          xs   = RE.collectSymbols $ combine (,) loc $ unS $ unlabel ns
      in (loc, Left ns) : concatMap (uncurry f) xs

-- | Returns the substrategy or rule at a strategy location. Nothing indicates that the location is invalid
subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (StrategyOrRule a)
subStrategy loc s =
   case loc of
      []   -> return (Left s) 
      n:ns -> 
         case lookup n . RE.collectSymbols . RE.withIndex . unS . unlabel $ s of
            Just (Left r)  |  null ns -> return (Right r)
            Just (Right t) -> subStrategy ns t
            _ -> Nothing

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules fun = f
 where
   f (LS n s)    = LS n (g s)
   g (S expr)    = S (fmap h expr)
   h (Left r)    = Left (fun r)
   h (Right ls)  = Right (f ls)

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: LabeledStrategy a -> [Rule a]
rulesInStrategy s = [ r | (_, Right r) <- strategyLocations s ]

-- local helper-function
combine :: ([Int] -> a -> b) -> [Int] -> RE.Grammar a -> RE.Grammar b
combine g is = fmap (\(i, a) -> g (is++[i]) a) . RE.withIndex

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f s = mapRules g (label (strategyName s) (doAfter f idRule <*> unlabel s))
 where
   g r | isMajorRule r = doAfter f r  
       | otherwise     = r

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of executed rules). A prefix
-- is still "aware" of the labels that appear in the strategy. A prefix is encoded as a list of integers 
-- (and can be reconstructed from such a list: see @makePrefix@).
data Prefix a = P [(Int, Step a)] (RE.Grammar (Step a))

instance Show (Prefix a) where
   show (P xs _) = show (map fst xs)

instance Eq (Prefix a) where
   P xs _ == P ys _ = map fst xs == map fst ys

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: [Int] -> LabeledStrategy a -> Prefix a
makePrefix is ls = rec [] is start
 where
   start = withMarks ls
   
   rec acc [] g = P (reverse acc) g
   rec acc (n:ns) g = 
      case drop n (RE.firsts g) of
         (z, h):_ -> rec ((n, z):acc) ns h
         _        -> P [] start

-- | The @Step@ data type can be used to inspect the structure of the strategy
data Step a = Begin StrategyLocation | Step StrategyLocation (Rule a) | End StrategyLocation
   deriving Show 

instance Apply Step where
   apply (Step _ r) = apply r
   apply (Begin _)  = return
   apply (End _)    = return

-- | Complete the remaining strategy
runPrefix :: Prefix a -> a -> [(a, Prefix a)]
runPrefix = runPrefixUntil (const False)

-- | Continue with a prefix until a certain condition is fulfilled
runPrefixUntil :: (Step a -> Bool) -> Prefix a -> a -> [(a, Prefix a)]
runPrefixUntil stop (P xs0 g0) a0 = 
   let f (a, g, xs1) = (a, P (xs0++xs1) g)
   in map f (runPrefixUntilHelper stop a0 g0)

-- local helper
runPrefixUntilHelper :: (Step a -> Bool) -> a -> RE.Grammar (Step a) -> [(a, RE.Grammar (Step a), [(Int, Step a)])]
runPrefixUntilHelper stop a g
   | RE.empty g = [(a, g, [])]
   | otherwise  = concat (zipWith f [0..] (RE.firsts g))
 where
   add n s this = [ (a, g, (n,s):xs) | (a, g, xs) <- this ]
   f n (step, h) = add n step $ 
      case step of
         Begin _  -> recStop a h 
         End _    -> recStop a h
         Step _ r -> [ result | b <- applyAll r a, result <- recStop b h ]
    where
      recStop a g
         | stop step = [(a, g, [])]
         | otherwise = runPrefixUntilHelper stop a g
 
-- | Continue with a prefix until the next major rule
runPrefixMajor :: Prefix a -> a -> [(a, Prefix a)]
runPrefixMajor = runPrefixUntil stop
 where
   stop (Step _ r) = isMajorRule r
   stop _          = False  
   
-- | Continue with a prefix until a certain strategy location is reached. At least one
-- major rule should have been executed
runPrefixLocation :: StrategyLocation -> Prefix a -> a -> [(a, Prefix a)]
runPrefixLocation loc p0 = concatMap check . runPrefixUntil stop p0
 where
   stop (End is)    = is==loc
   stop (Step is _) = is==loc
   stop _           = False
   
   check result@(a, p)
      | null rules            = [result]
      | all isMinorRule rules = runPrefixLocation loc p a
      | otherwise             = [result]
    where
      rules = stepsToRules $ drop (length $ prefixToSteps p0) $ prefixToSteps p

-- | Returns the steps that belong to the prefix
prefixToSteps :: Prefix a -> [Step a]
prefixToSteps (P xs _) = map snd xs
 
-- | Retrieves the rules from a list of steps
stepsToRules :: [Step a] -> [Rule a]
stepsToRules steps = [ r | Step _ r <- steps ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastRuleInPrefix :: Prefix a -> Maybe (Rule a)
lastRuleInPrefix = safeHead . reverse . stepsToRules . prefixToSteps

-- local helper function
withMarks :: LabeledStrategy a -> RE.Grammar (Step a)
withMarks = rec [] 
 where
   rec is = mark is . RE.join . combine f is . unS . unlabel
   f   is = either (RE.symbol . Step is) (rec is)
   mark is g = 
      let begin = RE.symbol (Begin is)
          end   = RE.symbol (End is) 
      in begin RE.<*> g RE.<*> end