module Common.Strategy.Grammar
   ( Step(..), makeTree, treeCore
   ) where

import Common.Classes
import Common.Derivation
import Common.Strategy.Core hiding (emptyEnv)
import Common.Transformation
import Data.Maybe
import qualified Data.IntMap as IM

data Step l a = Enter l | Exit l | RuleStep (Rule a)
   deriving Show

instance Apply (Step l) where
   applyAll (RuleStep r) = applyAll r
   applyAll _            = return

----------------------------------------------------------------------
-- Abstract data type

data State l a = S 
   { grammar     :: Core l a
   , stack       :: [Either l (Core l a, Env l a)]
   , environment :: Env l a
   }

makeTree :: Core l a -> DerivationTree (Step l a) ()
makeTree g = f (S g [] emptyEnv)
 where
   f st = addBranches list node
    where
      node = singleNode () (empty st)
      list = [ (step, f rest) | (step, rest) <- firsts st ]

----------------------------------------------------------------------
-- Elementary operations

-- | Tests whether the grammar accepts the empty string
empty :: State l a -> Bool
empty state = 
   let p (m, s) = isNothing m && empty s
   in isEmptyState state || any p (smallStep state)

isEmptyState :: State l a -> Bool
isEmptyState state = 
   case grammar state of
      Succeed -> null (stack state)
      _       -> False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: State l a -> [(Step l a, State l a)]
firsts = concatMap f . smallStep
 where
   f (Nothing, s)   = firsts s
   f (Just step, s) = [(step, s)]

smallStep :: State l a -> [(Maybe (Step l a), State l a)]
smallStep state =
   case grammar state of
      s :*: t ->  [(Nothing, state {grammar = s, stack = Right (t, environment state) : stack state})]
      s :|: t ->  replaceBy s ++ replaceBy t
      Rec i s -> [(Nothing, state {grammar = s, environment = addToEnv i s (environment state) })]
      Var i -> case findInEnv i (environment state) of
                  Just (e, s)  -> [(Nothing, (state {grammar = s, environment = e}))]
                  Nothing -> error "free var in core expression"
      Rule  Nothing a ->  [(Just (RuleStep a), succeed)]
      Rule  (Just l) a -> replaceBy (Label l (Rule Nothing a))
      Label l s -> [(Just (Enter l), state {grammar = s, stack = Left l : stack state})]
      Not s -> replaceBy (Rule Nothing (notRule (environment state) (noLabels s)))
      s :|>: t -> replaceBy (s :|: (Not (noLabels s) :*: t))
      Many s   -> replaceBy (coreMany s)
      Repeat s -> replaceBy (coreRepeat s)
      Fail     -> []
      Succeed -> case stack state of 
              Right (x,e):xs -> [(Nothing, state {grammar = x, stack = xs, environment = e})]
              Left l:xs -> [(Just (Exit l), state {stack = xs})]
              [] -> []
 where
   succeed = state {grammar = Succeed}
   replaceBy s = return (Nothing, state {grammar = s})

notRule :: Env l a -> Core l a -> Rule a
notRule env core = checkRule $ null . runCoreWith env core

instance Apply (Core l) where
   applyAll = runCore

treeCore :: Core l a -> a -> DerivationTree (Step l a) a
treeCore = treeCoreWith emptyEnv

treeCoreWith :: Env l a -> Core l a -> a -> DerivationTree (Step l a) a
treeCoreWith env core = treeState (S core [] env)

treeState :: State l a -> a -> DerivationTree (Step l a) a
treeState state a = addBranches list node
 where
   node = singleNode a (empty state)
   list = [ (step, treeState s b) | (step, s) <- firsts state, b <- applyAll step a ]
   
runCore :: Core l a -> a -> [a]
runCore = runCoreWith emptyEnv

runCoreWith :: Env l a -> Core l a -> a -> [a]
runCoreWith env core = results . treeCoreWith env core

----------------------------------------------------------------------
-- Local helper functions and instances

newtype Env l a = Env (IM.IntMap (Env l a, Core l a))

emptyEnv :: Env l a
emptyEnv = Env IM.empty
  
addToEnv :: Int -> Core l a -> Env l a -> Env l a
addToEnv n a env@(Env m) = Env (IM.insert n (env, a) m)
  
findInEnv :: Int -> Env l a -> Maybe (Env l a, Core l a)
findInEnv n (Env m) = do
   (e, a) <- IM.lookup n m
   return (e, Rec n a)