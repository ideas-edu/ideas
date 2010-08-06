module Common.Strategy.Grammar
   ( Step(..), stateTree, treeCore, makeState, State, trace, value, choices, replay
   , firsts
   ) where

import Common.Classes
import Common.Derivation
import Common.Strategy.Core hiding (emptyEnv)
import Common.Transformation
import Control.Arrow
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
   , choices     :: [Bool]
   , trace       :: [Step l a]
   , value       :: Maybe a
   }
 deriving Show

makeState :: Core l a -> State l a
makeState core = S core [] emptyEnv [] [] Nothing

treeCore :: Core l a -> a -> DerivationTree (Step l a) a
treeCore core a = fmap (fromJust . value) (stateTree (makeState core) {value = Just a})

stateTree :: State l a -> DerivationTree (Step l a) (State l a)
stateTree state = addBranches list node
 where
   node = singleNode state (empty state)
   list = map (second stateTree) (firsts state)

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

replay :: Monad m => Int -> [Bool] -> State l a -> m (State l a)
replay 0 _  state = return state
replay n bs state =
   case grammar state of
      Rule r  -> replay (n-1) bs state {grammar = Succeed, trace = RuleStep r : trace state}
      s :|: t -> case bs of
                    x:xs -> replay n xs state {grammar = if x then s else t, choices = x : choices state}
                    []   -> fail "replay failed"
      _       -> case smallStep state of
                    [(m, new)] -> replay (if isJust m then n-1 else n) bs new
                    _          -> fail "replay failed"

smallStep :: State l a -> [(Maybe (Step l a), State l a)]
smallStep state =
   update $
   case grammar state of
      s :*: t   -> [(Nothing, state {grammar = s, stack = Right (t, environment state) : stack state})]
      s :|: t   -> chooseFor True s ++ chooseFor False t
      Rec i s   -> [(Nothing, state {grammar = s, environment = addToEnv i s (environment state) })]
      Var i     -> case findInEnv i (environment state) of
                      Just (e, s)  -> [(Nothing, (state {grammar = s, environment = e}))]
                      Nothing -> error "free var in core expression"
      Rule  r   -> case value state of
                      Just a  -> [ (Just (RuleStep r), state {grammar = Succeed, value = Just b}) 
                                 | b <- applyAll r a ]
                      Nothing -> [(Just (RuleStep r), succeed)]
      Label l s -> [(Just (Enter l), state {grammar = s, stack = Left l : stack state})]
      Not s     -> replaceBy (Rule (notRule (environment state) (noLabels s)))
      s :|>: t  -> replaceBy (s :|: (Not s :*: t))
      Many s    -> replaceBy (coreMany s)
      Repeat s  -> replaceBy (coreRepeat s)
      Fail      -> []
      Succeed   -> case stack state of 
                      Right (x,e):xs -> [(Nothing, state {grammar = x, stack = xs, environment = e})]
                      Left l:xs -> [(Just (Exit l), state {stack = xs})]
                      [] -> []
 where
   succeed = state {grammar = Succeed}
   replaceBy s = return (Nothing, state {grammar = s})
   update = map (\(mstep, s) -> (mstep, maybe s (\a -> s {trace = a : trace s}) mstep))
   chooseFor b a = map (second (\s -> s {choices = b : choices s})) (replaceBy a)

notRule :: Env l a -> Core l a -> Rule a
notRule env core = checkRule $ \a -> 
   null $ results $ stateTree $ (makeState core) {environment = env, value = Just a}

instance Apply (Core l) where
   applyAll core = results . treeCore core

----------------------------------------------------------------------
-- Local helper functions and instances

newtype Env l a = Env (IM.IntMap (Env l a, Core l a)) deriving Show

emptyEnv :: Env l a
emptyEnv = Env IM.empty
  
addToEnv :: Int -> Core l a -> Env l a -> Env l a
addToEnv n a env@(Env m) = Env (IM.insert n (env, a) m)
  
findInEnv :: Int -> Env l a -> Maybe (Env l a, Core l a)
findInEnv n (Env m) = do
   (e, a) <- IM.lookup n m
   return (e, Rec n a)