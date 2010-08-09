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

instance Apply (Core l) where
   applyAll = runCore

----------------------------------------------------------------------
-- Abstract data type

data State l a = S
   { stack       :: [Either l (Core l a, Env l a)]
   , environment :: Env l a
   , choices     :: [Bool]
   , trace       :: [Step l a]
   , value       :: a
   }

makeState :: Core l a -> a -> State l a
makeState core a = push core (S [] emptyEnv [] [] a)

treeCore :: Core l a -> a -> DerivationTree (Step l a) a
treeCore core = fmap value . stateTree . makeState core

stateTree :: State l a -> DerivationTree (Step l a) (State l a)
stateTree state = addBranches list node
 where
   node = singleNode state (empty state)
   list = map (second stateTree) (firsts state)

----------------------------------------------------------------------
-- Elementary operations

empty :: State l a -> Bool
empty state = 
   let p (m, s) = isNothing m && empty s
   in isEmptyState state || any p (smallStep state)

isEmptyState :: State l a -> Bool
isEmptyState = null . stack

firsts :: State l a -> [(Step l a, State l a)]
firsts = concatMap f . smallStep
 where
   f (Nothing, s)   = firsts s
   f (Just step, s) = [(step, s)]

replay :: Monad m => Int -> [Bool] -> Core l a -> m (State l a)
replay n bs core = rec n bs (makeState core noValue)
 where
   noValue = error "no value in replay"
 
   rec 0 _  state = return state
   rec n bs state = 
      case pop state of
         Nothing              -> return state
         Just (Left l, s)     -> rec (n-1) bs (s {trace = Exit l : trace s})
         Just (Right core, s) ->
            case core of
               Rule r  -> rec (n-1) bs s {trace = RuleStep r : trace s}
               Not _   -> rec n bs s
               a :|: b -> 
                  case bs of
                     x:xs -> rec n xs (makeChoice x (if x then push a s else push b s))
                     []   -> fail "replay failed"
               _ -> 
                  case smallCoreStep core s of
                     [(m, new)] -> rec (if isJust m then n-1 else n) bs new
                     _          -> fail "replay failed"

smallStep :: State l a -> [(Maybe (Step l a), State l a)]
smallStep state = 
   case pop state of 
      Nothing              -> []
      Just (Left l, s)     -> [(Just (Exit l), s {trace = Exit l : trace s})]
      Just (Right core, s) -> smallCoreStep core s
      
smallCoreStep :: Core l a -> State l a -> [(Maybe (Step l a), State l a)]
smallCoreStep core state = 
   case core of
      s :*: t   -> [(Nothing, push s (push t state))]
      s :|: t   -> chooseFor True s ++ chooseFor False t
      Rec i s   -> [(Nothing, push s state {environment = addToEnv i s (environment state) })]
      Var i     -> case findInEnv i (environment state) of
                      Just (e, s)  -> [(Nothing, (push s state {environment = e}))]
                      Nothing -> error "free var in core expression"
      Rule  r   -> hasStep (RuleStep r) [ state {value = b} | b <- applyAll r (value state) ]
      Label l s -> hasStep (Enter l) [push s (pushExit l state)]
      Not s     -> if null (runCoreWith (environment state) s (value state))
                   then replaceBy Succeed
                   else []
      s :|>: t  -> replaceBy (s :|: (Not s :*: t))
      Many s    -> replaceBy (coreMany s)
      Repeat s  -> replaceBy (coreRepeat s)
      Fail      -> []
      Succeed   -> [(Nothing, state)]
 where
   replaceBy s = return (Nothing, push s state)
   chooseFor b a = map (second (makeChoice b)) (replaceBy a)
   hasStep step xs = [ (Just step, s {trace = step : trace s}) | s <- xs ]

runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

runCoreWith :: Env l a -> Core l a -> a -> [a]
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
         Rec i s   -> runStep s state {environment = addToEnv i s (environment state)}
         Var i     -> case findInEnv i (environment state) of
                         Just (e, s)  -> runStep s state {environment = e}
                         Nothing -> error "free var in core expression"
         Rule  r   -> [ s | b <- applyAll r (value state), s <- runState state {value = b} ]
         Label _ s -> runStep s state
         Not s     -> if null (runCoreWith (environment state) s (value state))
                      then runState state
                      else []
         s :|>: t  -> let xs = runStep s state
                      in if null xs then runStep t state else xs
         Many s    -> runStep (coreMany s) state
         Repeat s  -> runStep (coreRepeat s) state
         Fail      -> []
         Succeed   -> runState state

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
   
push :: Core l a -> State l a -> State l a
push core s = s {stack = Right (core, environment s) : stack s}

pushExit :: l -> State l a -> State l a
pushExit l s = s {stack = Left l : stack s}

pop :: State l a -> Maybe (Either l (Core l a), State l a)
pop s = case stack s of
           []                 -> Nothing
           x:xs -> Just (either f g x s {stack = xs})
 where
   f l         s = (Left l, s)
   g (core, e) s = (Right core, s {environment = e})
   
makeChoice :: Bool -> State l a -> State l a
makeChoice b s = s {choices = b : choices s}