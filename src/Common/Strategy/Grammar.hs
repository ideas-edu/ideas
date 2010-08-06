module Common.Strategy.Grammar
   ( Step(..), stateTree, treeCore, State, trace, value, choices, replay
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
   { current     :: Core l a
   , stack       :: [Either l (Core l a, Env l a)]
   , environment :: Env l a
   , choices     :: [Bool]
   , trace       :: [Step l a]
   , value       :: a
   }

makeState :: Core l a -> a -> State l a
makeState core a = S core [] emptyEnv [] [] a

treeCore :: Core l a -> a -> DerivationTree (Step l a) a
treeCore core = fmap value . stateTree . makeState core

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
   case current state of
      Succeed -> null (stack state)
      _       -> False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: State l a -> [(Step l a, State l a)]
firsts = concatMap f . smallStep
 where
   f (Nothing, s)   = firsts s
   f (Just step, s) = [(step, s)]

replay :: Monad m => Int -> [Bool] -> Core l a -> m (State l a)
replay n bs core = do
   let noValue = error "no value in replay" 
   s <- rec n bs (makeState core noValue)
   return s {choices = reverse bs}
 where
   rec 0 _  state = return state
   rec n bs state =
      case current state of
         Rule r  -> rec (n-1) bs state {current = Succeed, trace = RuleStep r : trace state}
         Not _   -> rec n bs state {current = Succeed}
         s :|: t -> case bs of
                       x:xs -> rec n xs state {current = if x then s else t}
                       []   -> fail "replay failed"
         _       -> case smallStep state of
                       [(m, new)] -> rec (if isJust m then n-1 else n) bs new
                       _          -> fail "replay failed"

smallStep :: State l a -> [(Maybe (Step l a), State l a)]
smallStep state =
   update $
   case current state of
      s :*: t   -> [(Nothing, state {current = s, stack = Right (t, environment state) : stack state})]
      s :|: t   -> chooseFor True s ++ chooseFor False t
      Rec i s   -> [(Nothing, state {current = s, environment = addToEnv i s (environment state) })]
      Var i     -> case findInEnv i (environment state) of
                      Just (e, s)  -> [(Nothing, (state {current = s, environment = e}))]
                      Nothing -> error "free var in core expression"
      Rule  r   -> [ (Just (RuleStep r), state {current = Succeed, value = b}) 
                   | b <- applyAll r (value state) ]
      Label l s -> [(Just (Enter l), state {current = s, stack = Left l : stack state})]
      Not s     -> let new = makeState (noLabels s) (value state) in 
                   if null (results (stateTree new))
                   then replaceBy Succeed
                   else []
      s :|>: t  -> replaceBy (s :|: (Not s :*: t))
      Many s    -> replaceBy (coreMany s)
      Repeat s  -> replaceBy (coreRepeat s)
      Fail      -> []
      Succeed   -> case stack state of 
                      Right (x,e):xs -> [(Nothing, state {current = x, stack = xs, environment = e})]
                      Left l:xs -> [(Just (Exit l), state {stack = xs})]
                      [] -> []
 where
   replaceBy s = return (Nothing, state {current = s})
   update = map (\(mstep, s) -> (mstep, maybe s (\a -> s {trace = a : trace s}) mstep))
   chooseFor b a = map (second (\s -> s {choices = b : choices s})) (replaceBy a)

instance Apply (Core l) where
   applyAll core = results . treeCore core

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