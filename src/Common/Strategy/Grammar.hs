module Common.Strategy.Grammar
   ( Step(..), stateTree, treeCore, State, trace, value, choices, replay
   , runCore, runCoreWith
   ) where

import Common.Classes
import Common.Derivation hiding (isEmpty)
import Common.Strategy.Core
import Common.Transformation
import Control.Monad
import Control.Arrow

data Step l a = Enter l | Exit l | RuleStep (Rule a)
   deriving Show

instance Apply (Step l) where
   applyAll (RuleStep r) = applyAll r
   applyAll _            = return

instance Apply (Core l) where
   applyAll = runCore

----------------------------------------------------------------------
-- Abstract data type

data State l a = S
   { stack       :: [Either l (CoreEnv l a, Core l a)]
   , environment :: CoreEnv l a
   , choices     :: [Bool]
   , trace       :: [Step l a]
   , value       :: a
   }

makeState :: Core l a -> a -> State l a
makeState core a = push core (S [] emptyCoreEnv [] [] a)

treeCore :: Core l a -> a -> DerivationTree (Step l a) a
treeCore core = fmap value . stateTree . makeState core

stateTree :: State l a -> DerivationTree (Step l a) (State l a)
stateTree state = addBranches list node
 where
   node = singleNode state (isEmpty state)
   list = map (second stateTree) (firsts state)

----------------------------------------------------------------------
-- Elementary operations

isEmpty :: State l a -> Bool
isEmpty = null . stack

replay :: Monad m => Int -> [Bool] -> Core l a -> m (State l a)
replay n bs core = rec n bs (makeState core noValue)
 where
   noValue = error "no value in replay"
 
   rec 0 _  state = return state
   rec n bs state = 
      case pop state of
         Nothing              -> return state
         Just (Left l, s)     -> rec (n-1) bs (s {trace = Exit l : trace s})
         Just (Right core, s) -> replayStep n bs core s
                     
   replayStep n bs core state =
      case core of
         s :*: t   -> rec n bs (push s (push t state))
         a :|: b -> 
            case bs of
               x:xs -> rec n xs (makeChoice x (if x then push a state else push b state))
               []   -> fail "replay failed"
         Rec i s   -> rec n bs (push s (addEnv i s state))
         Var i     -> withEnv (replayStep n bs) i state
         Rule r  -> rec (n-1) bs state {trace = RuleStep r : trace state}
         Label l s -> rec (n-1) bs (push s (pushExit l state {trace = Enter l : trace state}))
         Not _   -> rec n bs state
         s :|>: t  -> replayStep n bs (s :|: (Not s :*: t)) state
         Many s    -> replayStep n bs (coreMany s) state
         Repeat s  -> replayStep n bs (coreRepeat s) state
         Fail      -> fail "replay failed"
         Succeed   -> rec n bs state

firsts :: State l a -> [(Step l a, State l a)]
firsts state =
   case pop state of 
      Nothing              -> []
      Just (Left l, s)     -> [(Exit l, s {trace = Exit l : trace s})]
      Just (Right core, s) -> firstsStep core s
 where
   firstsStep core state =
      case core of
         s :*: t   -> firstsStep s (push t state)
         s :|: t   -> chooseFor True s ++ chooseFor False t
         Rec i s   -> firstsStep s (addEnv i s state)
         Var i     -> withEnv firstsStep i state
         Rule  r   -> hasStep (RuleStep r) (useRule r state)
         Label l s -> hasStep (Enter l) [push s (pushExit l state)]
         Not s     -> guard (checkNot s state) >> firsts state
         s :|>: t  -> firstsStep (s :|: (Not s :*: t)) state
         Many s    -> firstsStep (coreMany s) state
         Repeat s  -> firstsStep (coreRepeat s) state
         Fail      -> []
         Succeed   -> firsts state
    where
      chooseFor b core = firstsStep core (makeChoice b state)
      hasStep step xs = [ (step, s {trace = step : trace s}) | s <- xs ]

runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

runCoreWith :: CoreEnv l a -> Core l a -> a -> [a]
runCoreWith env core = runState . setEnv . makeState core
 where setEnv s = s {environment = env}

runState :: State l a -> [a]
runState state =
   case pop state of
      Nothing              -> [value state]
      Just (Left _, s)     -> runState s
      Just (Right core, s) -> runStep core s
 where
   runStep core state = 
      case core of
         s :*: t   -> runStep s (push t state)
         s :|: t   -> runStep s state ++ runStep t state
         Rec i s   -> runStep s (addEnv i s state)
         Var i     -> withEnv runStep i state
         Rule  r   -> concatMap runState (useRule r state)
         Label _ s -> runStep s state
         Not s     -> guard (checkNot s state) >> runState state
         s :|>: t  -> let xs = runStep s state
                      in if null xs then runStep t state else xs
         Many s    -> runStep (coreMany s) state
         Repeat s  -> runStep (coreRepeat s) state
         Fail      -> []
         Succeed   -> runState state

addEnv :: Int -> Core l a -> State l a -> State l a
addEnv i c s = s {environment = insertCoreEnv i c (environment s)}

withEnv :: (Core l a -> State l a -> b) -> Int -> State l a -> b
withEnv f i s = case lookupCoreEnv i (environment s) of
                         Just (e, a)  -> f a s {environment = e}
                         Nothing -> error "free var in core expression"

checkNot :: Core l a -> State l a -> Bool
checkNot core state = null (runCoreWith (environment state) core (value state))

useRule :: Rule a -> State l a -> [State l a]
useRule r state = [ state {value = b} | b <- applyAll r (value state) ]

----------------------------------------------------------------------
-- Local helper functions and instances
   
push :: Core l a -> State l a -> State l a
push core s = s {stack = Right (environment s, core) : stack s}

pushExit :: l -> State l a -> State l a
pushExit l s = s {stack = Left l : stack s}

pop :: State l a -> Maybe (Either l (Core l a), State l a)
pop s = case stack s of
           []                 -> Nothing
           x:xs -> Just (either f g x s {stack = xs})
 where
   f l         s = (Left l, s)
   g (e, core) s = (Right core, s {environment = e})
   
makeChoice :: Bool -> State l a -> State l a
makeChoice b s = s {choices = b : choices s}