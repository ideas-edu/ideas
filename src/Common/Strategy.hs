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
   , fullDerivationTree, derivationTree
     -- * Strategy combinators
     -- ** Basic combinators
   , (<*>), (<|>), succeed, fail, label, sequence, alternatives -- <||>
     -- ** EBNF combinators
   , many, many1, replicate, option
     -- ** Negation and greedy combinators
   , check, not, repeat, repeat1, try, (|>), exhaustive
     -- ** Traversal combinators
   , fix, once, somewhere, topDown, bottomUp
     -- * Strategy locations
   , StrategyLocation, StrategyOrRule, subStrategy, strategyLocations
   , mapRules, rulesInStrategy, cleanUpStrategy
     -- * Prefixes
   , Prefix, emptyPrefix, makePrefix, prefixTree, Step(..)
   , prefixToSteps, stepsToRules, lastStepInPrefix
   ) where

import Common.Apply
import Common.Context
import Common.Derivation
import Common.Rewriting hiding (inverse)
import Common.Transformation hiding (checkRule)
import Common.Uniplate hiding (somewhere)
import Common.Utils
import Prelude hiding (fail, not, repeat, replicate, sequence)
import qualified Common.Grammar as RE
import qualified Prelude as Prelude

-----------------------------------------------------------
-- Data types and type classes

-- | Abstract data type for a strategy
newtype Strategy a = S (S a)

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
   show (S s) = show (toStepGrammar s)

instance Show a => Show (LabeledStrategy a) where
   show s = 
      strategyName s ++ ": " ++ show (unlabel s)

-- instances for Apply
instance Apply Strategy where
   applyAll s = results . fullDerivationTree s

instance Apply LabeledStrategy where
   applyAll = applyAll . unlabel

-- instances for IsStrategy
instance IsStrategy RewriteRule where
   toStrategy r = 
      toStrategy (makeRule (ruleName r) (RewriteRule r))

instance IsStrategy Rule where
   toStrategy = S . Rule

instance IsStrategy Strategy where
   toStrategy = id
   
instance IsStrategy (LabeledStrategy) where
  toStrategy (LS n (S s)) = S (Label n s)

-----------------------------------------------------------
--- Running strategies

-- | Returns the derivation tree for a strategy and a term, including all
-- minor rules
fullDerivationTree :: IsStrategy f => f a -> a -> DerivationTree (Rule a) a
fullDerivationTree f = let (S s) = toStrategy f in makeTree (toGrammar s)

-- | Returns the derivation tree for a strategy and a term with only major rules
derivationTree :: IsStrategy f => f a -> a -> DerivationTree (Rule a) a
derivationTree s = mergeSteps isMajorRule . fullDerivationTree s

-----------------------------------------------------------
--- Strategy combinators

-- Basic combinators --------------------------------------

infixr 3 <|>
--infixr 4 <||>
infixr 5 <*>

-- | Put two strategies in sequence (first do this, then do that)
(<*>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <*> t = S (toS s :*: toS t)

-- | Choose between the two strategies (either do this or do that)
(<|>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
s <|> t = S (toS s :|: toS t)

-- | Run two strategies in parallel (with interleaving)
-- (<||>) :: (IsStrategy f, IsStrategy g) => f a -> g a -> Strategy a
--s <||> t = makeS (unS (toStrategy s) RE.<||> unS (toStrategy t))

-- | The strategy that always succeeds (without doing anything)
succeed :: Strategy a
succeed = S Succeed

-- | The strategy that always fails
fail :: Strategy a
fail = S Fail

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
many s = S (Many (toS s))

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
check p = toStrategy (checkRule p)

-- | Check whether or not the argument strategy cannot be applied: the result
--   strategy only succeeds if this is not the case (otherwise it fails).
not :: IsStrategy f => f a -> Strategy a
not s = S (Not (toS s))

{- alternative definition, with an early commit. No performance gain was
measurable

applicableOne :: Strategy a -> a -> Bool
applicableOne s a = 
   let tree = derivationTree s a
   in endpoint tree || Prelude.not (null (branches tree)) -}

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
s |> t = S (toS s :|>: toS t) -- s <|> (not s <*> t)

-- | Apply the strategies from the list exhaustively (until this is no longer possible)
exhaustive :: IsStrategy f => [f a] -> Strategy a
exhaustive = repeat . alternatives

-- Traversal combinators --------------------------------------------

-- | A fix-point combinator on strategies (to model recursion). Powerful
-- (but dangerous) combinator
fix :: (Strategy a -> Strategy a) -> Strategy a
fix f = S (fixS (\s -> let S r = f (S s) in r)) 

-- | Apply a strategy on (exactly) one of the term's direct children
once :: (IsStrategy f, Uniplate a) => f (Context a) -> Strategy (Context a)
once s = ruleMoveDown <*> s <*> ruleMoveUp
 where
   ruleMoveDown = minorRule $ makeSimpleRuleList "MoveDown" moveDown
   moveDown c = 
      let n = maybe 0 (pred . length . children) (currentFocus c)
      in [ changeLocation (locationDown i) c | i <- [0 .. n] ]
   
   ruleMoveUp = minorRule $ makeSimpleRule "MoveUp" moveUp
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

-- | Returns a list of all strategy locations, paired with the labeled 
-- substrategy or rule at that location

strategyLocations :: LabeledStrategy a -> [(StrategyLocation, Either (LabeledStrategy a) (Rule a))]
strategyLocations = rec [] 
 where
   rec loc ls@(LS _ (S s)) = 
      let f i (Left (l,s)) = rec (loc++[i]) (LS l (S s))
          f i (Right r)    = [(loc++[i], Right r)]
      in (loc, Left ls) : concat (zipWith f [0..] (collectS s))

-- | Returns the substrategy or rule at a strategy location. Nothing indicates that the location is invalid
subStrategy :: StrategyLocation -> LabeledStrategy a -> Maybe (StrategyOrRule a)
subStrategy loc ls@(LS _ (S s)) =
   case loc of
      []   -> return (Left ls) 
      n:ns -> 
         case drop n (collectS s) of
            Left (l, s):_ -> subStrategy ns (LS l (S s))
            Right r:_ | null ns -> Just (Right r)
            _ -> Nothing

-- | Apply a function to all the rules that make up a labeled strategy
mapRules :: (Rule a -> Rule b) -> LabeledStrategy a -> LabeledStrategy b
mapRules f (LS n (S s)) = LS n (S (mapS f s))

-- | Returns a list of all major rules that are part of a labeled strategy
rulesInStrategy :: LabeledStrategy a -> [Rule a]
rulesInStrategy (LS _ (S s)) = [ r | Rule r <- universe (removeNot s), isMajorRule r ]

-- | Use a function as do-after hook for all rules in a labeled strategy
cleanUpStrategy :: (a -> a) -> LabeledStrategy a -> LabeledStrategy a
cleanUpStrategy f s = mapRules g (label (strategyName s) (doAfter f idRule <*> unlabel s))
 where
   g r | isMajorRule r = doAfter f r  
       | otherwise     = r

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of 
-- executed rules). A prefix is still "aware" of the labels that appear in the 
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed 
-- from such a list: see @makePrefix@). The list is stored in reversed order.
data Prefix a = P [(Int, Step a)] (RE.Grammar (Step a))

instance Show (Prefix a) where
   show (P xs _) = show (reverse (map fst xs))

instance Eq (Prefix a) where
   P xs _ == P ys _ = map fst xs == map fst ys

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: [Int] -> LabeledStrategy a -> Prefix a
makePrefix is ls = rec [] is start
 where
   start = withSteps ls
   
   rec acc [] g = P acc g
   rec acc (n:ns) g = 
      case drop n (RE.firsts g) of
         (z, h):_ -> rec ((n, z):acc) ns h
         _        -> P [] start

-- | The @Step@ data type can be used to inspect the structure of the strategy
data Step a = Begin StrategyLocation 
            | Step (Maybe StrategyLocation) (Rule a) 
            | End StrategyLocation
   deriving (Show, Eq)

instance Apply Step where
   applyAll (Step _ r) = applyAll r
   applyAll (Begin _)  = return
   applyAll (End _)    = return

instance Apply Prefix where
   applyAll p = results . prefixTree p

-- | Create a derivation tree with a "prefix" as annotation.
prefixTree :: Prefix a -> a -> DerivationTree (Prefix a) a
prefixTree (P xs g) a =
   addBranches list (singleNode a (RE.empty g))
 where
   add (i, (step, rest)) = P ((i, step):xs) rest
   list = [ (newPrefix, prefixTree newPrefix b)
          | triple@(_, (step, _)) <- zip [0..] (RE.firsts g)
          , let newPrefix = add triple
          , b <- applyAll step a
          ]
 
-- | Returns the steps that belong to the prefix
prefixToSteps :: Prefix a -> [Step a]
prefixToSteps (P xs _) = map snd (reverse xs)
 
-- | Retrieves the rules from a list of steps
stepsToRules :: [Step a] -> [Rule a]
stepsToRules steps = [ r | Step _ r <- steps ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (Step a)
lastStepInPrefix (P xs _) = safeHead (map snd xs)

-- local helper function
withSteps :: LabeledStrategy a -> RE.Grammar (Step a)
withSteps (LS _ (S s)) = toStepGrammar s {- rec []
 where
   rec is = mark is . RE.join . combine f is . unS . unlabel
   f   is = either (RE.symbol . Step is) (rec is)
   mark is g = 
      let begin = RE.symbol (Begin is)
          end   = RE.symbol (End is) 
      in begin RE.<*> g RE.<*> end -}
      
--------------------------------------------
-- New

data S a = S a :*:  S a 
         | S a :|:  S a
         | S a :|>: S a
         | Many (S a)
         | Succeed 
         | Fail
         | Label String (S a)
         | Rule (Rule a)
         | Var Int
         | Rec Int (S a)
         | Not (S a)

instance Uniplate (S a) where
   uniplate strategy =
      case strategy of
         a :*: b   -> ([a,b], \[x,y] -> x :*: y)
         a :|: b   -> ([a,b], \[x,y] -> x :|: y)
         a :|>: b  -> ([a,b], \[x,y] -> x :|>: y)
         Many a    -> ([a], \[x] -> Many x)
         Not a     -> ([a], \[x] -> Not x)
         Label l a -> ([a], \[x] -> Label l x)
         Rec n a   -> ([a], \[x] -> Rec n x)
         _         -> ([], \_ -> strategy)

toGrammar :: S a -> RE.Grammar (Rule a)
toGrammar strategy = {-RE.join . fmap f . toStepGrammar
 where
   f (Step _ r) = RE.symbol r
   f _ = RE.succeed -}
   case strategy of
      a :*: b   -> toGrammar a RE.<*> toGrammar b
      a :|: b   -> toGrammar a RE.<|> toGrammar b
      a :|>: b  -> toGrammar (a :|: (Not a :*: b))
      Many a    -> RE.many (toGrammar a)
      Succeed   -> RE.succeed
      Fail      -> RE.fail
      Label _ a -> toGrammar a
      Rule r    -> RE.symbol r
      Var n     -> RE.var n
      Rec n a   -> RE.rec n (toGrammar a)
      Not a     -> RE.symbol (checkRule (Prelude.not . applicable (S a)))

toStepGrammar :: S a -> RE.Grammar (Step a)
toStepGrammar = mark [] . rec 0 []
 where
   mark loc g = RE.symbol (Begin loc) RE.<*> g RE.<*> RE.symbol (End loc)
   rec i loc strategy = 
      case strategy of
         a :*: b   -> rec i loc a RE.<*> rec (i+countS a) loc b
         a :|: b   -> rec i loc a RE.<|> rec (i+countS a) loc b
         a :|>: b  -> rec i loc (a :|: (Not a :*: b))
         Many a    -> RE.many (rec i loc a)
         Succeed   -> RE.succeed
         Fail      -> RE.fail
         Label _ a -> let here = loc++[i]
                      in mark here (rec 0 here a)
         Rule r    -> let mloc = if isMajorRule r then Just (loc++[i]) else Nothing
                      in RE.symbol (Step mloc r)
         Var n     -> RE.var n
         Rec n a   -> RE.rec n (rec i loc a)
         Not a     -> RE.symbol (Step Nothing (checkRule (Prelude.not . applicable (S a))))

mapS :: (Rule a -> Rule b) -> S a -> S b
mapS f strategy =
   case strategy of
      a :*: b   -> mapS f a :*: mapS f b
      a :|: b   -> mapS f a :|: mapS f b
      a :|>: b  -> mapS f a :|>: mapS f b
      Many a    -> Many (mapS f a)
      Succeed   -> Succeed
      Fail      -> Fail
      Label l a -> Label l (mapS f a)
      Rule r    -> Rule (f r)
      Var n     -> Var n
      Rec n a   -> Rec n (mapS f a)
      Not a     -> Not (mapS f a)

countS :: S a -> Int
countS = length . collectS

removeNot :: S a -> S a
removeNot (Not _) = Succeed
removeNot s = f (map removeNot cs)
 where (cs, f) = uniplate s

-- For now: just major rules??
collectS :: S a -> [Either (String, S a) (Rule a)]
collectS (Rule r) = [Right r | isMajorRule r]
collectS (Label l a) = [Left (l, a)]
collectS (Not _) = [] -- [Right $ checkRule (Prelude.not . applicable (S a))]
collectS s = concatMap collectS (children s)

makeTree :: Apply f => RE.Grammar (f a) -> a -> DerivationTree (f a) a
makeTree s a  = addBranches (list s a) (singleNode a (RE.empty s))
 where
   list s a = [ (f, makeTree rest b)
              | (f, rest) <- RE.firsts s
              , b <- applyAll f a 
              ]

checkRule :: (a -> Bool) -> Rule a 
checkRule p = minorRule $ makeSimpleRule "check" $ \a ->
   if p a then Just a else Nothing

toS :: IsStrategy f => f a -> S a 
toS a = let (S s) = toStrategy a in s

fixS :: (S a -> S a) -> S a
fixS f = Rec i (f (Var i)) -- disadvantage: function f is applied twice
 where
   s = allVars (f (Rule idRule))
   i = if null s then 0 else maximum s + 1
   
allVars :: S a -> [Int]
allVars s = [ n | Rec n _ <- universe s ] ++ [ n | Var n <- universe s ]