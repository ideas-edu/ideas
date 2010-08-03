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
-- A prefix encodes a sequence of steps already performed (a so-called trace), 
-- and allows to continue the derivation at that particular point.
--
-----------------------------------------------------------------------------
module Common.Strategy.Prefix 
   ( Prefix, emptyPrefix, makePrefix
   , Step(..), prefixToSteps, prefixTree, stepsToRules, lastStepInPrefix
   ) where

import Common.Classes
import Common.Uniplate
import Common.Utils
import Common.Strategy.Abstract
import Common.Strategy.Core
import qualified Common.Strategy.Grammar as Grammar
import Common.Strategy.Grammar (Grammar, (<*>), (<|>), symbol)
import Common.Transformation
import Common.Derivation
import Common.Strategy.Location
import Common.Strategy.Parsing (Step(..))
import Data.Maybe

-----------------------------------------------------------
--- Prefixes

-- | Abstract data type for a (labeled) strategy with a prefix (a sequence of 
-- executed rules). A prefix is still "aware" of the labels that appear in the 
-- strategy. A prefix is encoded as a list of integers (and can be reconstructed 
-- from such a list: see @makePrefix@). The list is stored in reversed order.
data Prefix a = P [(Int, PStep a)] (DerivationTree (PStep a) ())

type PStep = Step (StrategyLocation, LabelInfo)

instance Show (Prefix a) where
   show (P xs _) = show (reverse (map fst xs))

instance Eq (Prefix a) where
   P xs _ == P ys _ = map fst xs == map fst ys

-- | Construct the empty prefix for a labeled strategy
emptyPrefix :: LabeledStrategy a -> Prefix a
emptyPrefix = fromMaybe (error "emptyPrefix") . makePrefix []

-- | Construct a prefix for a given list of integers and a labeled strategy.
makePrefix :: Monad m => [Int] -> LabeledStrategy a -> m (Prefix a)
makePrefix is ls = rec [] is start
 where
   mkCore = processLabelInfo snd . addLocation . toCore . toStrategy
   start  = strategyTree (mkCore ls)
 
   rec acc [] t = return (P acc t)
   rec acc (n:ns) t =
      case drop n (branches t) of
         (step, st):_ -> rec ((n, step):acc) ns st
         _            -> fail ("invalid prefix: " ++ show is)

instance Apply Prefix where
   applyAll p = results . prefixTree p

-- | Create a derivation tree with a "prefix" as annotation.
prefixTree :: Prefix a -> a -> DerivationTree (Prefix a) a
prefixTree (P xs t) = changeLabel snd . runTree (decorate xs t)

decorate :: [(Int, PStep a)] -> DerivationTree (PStep a) () -> DerivationTree (PStep a) (Prefix a)
decorate xs t =
   let list = zipWith make [0..] (branches t)
       make i (s, st) = (s, decorate ((i,s):xs) st)
   in addBranches list (singleNode (P xs t) (endpoint t))
 
-- | Returns the steps that belong to the prefix
prefixToSteps :: Prefix a -> [PStep a]
prefixToSteps (P xs _) = [ step | (_, step) <- reverse xs ]
 
-- | Retrieves the rules from a list of steps
stepsToRules :: [Step l a] -> [Rule a]
stepsToRules steps = [ r | RuleStep _ r <- steps ]

-- | Returns the last rule of a prefix (if such a rule exists)
lastStepInPrefix :: Prefix a -> Maybe (PStep a)
lastStepInPrefix (P xs _) = safeHead [ step | (_, step) <- xs ]

-------------------------------------------------------------------
-- Copied from core

strategyTree :: Core (StrategyLocation, LabelInfo) a -> DerivationTree (PStep a) ()
strategyTree = grammarTree . toGrammar

grammarTree :: Grammar a -> DerivationTree a ()
grammarTree gr = addBranches list node
 where 
   node = singleNode () (Grammar.empty gr)
   list = [ (f, grammarTree rest) | (f, rest) <- Grammar.firsts gr ]

runTree :: Apply f => DerivationTree (f a) info -> a -> DerivationTree (f a, info) a
runTree t a = addBranches list (singleNode a (endpoint t))
 where
   list = concatMap make (branches t)
   make (f, st) = [ ((f, root st), runTree st b) | b <- applyAll f a ]

toGrammar :: Core (StrategyLocation, LabelInfo) a -> Grammar (PStep a)
toGrammar core = recWith emptyEnv (checkCoreFree "toGrammar.top" [] core)
 where
   recWith env core =
      case core of
         a :*: b   -> rec a <*> rec b
         a :|: b   -> rec a <|> rec b
         a :|>: b  -> rec (a :|: (Not (noLabels a) :*: b))
         Many a    -> rec (coreMany a)
         Repeat a  -> rec (coreRepeat a)
         Succeed   -> Grammar.succeed
         Fail      -> Grammar.fail
         Label l a -> forLabel l (rec a)
         Rule ml r -> (maybe id forLabel ml) (symbol (RuleStep Nothing r))
         Var n     -> Grammar.var n
         Rec n a   -> Grammar.rec n (recWith (insertEnv n core env) a)
         Not a     -> symbol (RuleStep Nothing (notRule (checkCoreFree "toGrammar.Not" [] (replaceVars env (noLabels a)))))
    where
      rec = recWith env
      
   forLabel (loc, i) g =
      symbol (Enter (loc, i)) <*> g <*> symbol (Exit (loc, i))
         
notRule :: Apply f => f a -> Rule a
notRule f = checkRule (not . applicable f)

-- Safety check
checkCoreFree :: String -> [Int] -> Core l a -> Core l a
checkCoreFree msg ns core =
   case core of
      Rec n a -> Rec n (checkCoreFree msg (n:ns) a)
      Var n -> if n `elem` ns then core else error ("checkFree! " ++ msg)
      _ -> f (map (checkCoreFree msg ns) cs)
 where
   (cs, f) = uniplate core 