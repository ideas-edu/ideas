-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Basic machinery for executing a core strategy expression.
--
-----------------------------------------------------------------------------
module Common.Strategy.Parsing
   ( Step(..)
   , State, makeState, stack, choices, trace, value
   , parseDerivationTree, replay, runCore
   , firsts, Result(..), isReady
   ) where

import Common.Binding
import Common.Classes
import Common.DerivationTree
import Common.Strategy.Core
import Common.Rule
import Common.Utils.Uniplate
import Control.Arrow
import Control.Monad
import Data.Monoid

----------------------------------------------------------------------
-- Step data type

data Step l a = Enter l | Exit l | RuleStep Environment (Rule a) 
   deriving (Show, Eq)

-- A core expression where the symbols are steps instead of "only" rules
type StepCore l a = GCore l (Step l a)

instance Apply (Step l) where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return
   
----------------------------------------------------------------------
-- State data type

data State l a = S
   { stack   :: Stack l a
   , choices :: [Bool]
   , trace   :: [Step l a]
   , timeout :: !Int
   , value   :: a
   } deriving Show

data Stack l a = Stack
   { active    :: [StepCore l a] -- the active items, performed in sequence
   , suspended :: [StepCore l a] -- suspended items, performed after a step from active
   , remainder :: [StepCore l a] -- remaining items: must be empty if there are no suspended items
   } deriving Show

makeState :: Core l a -> a -> State l a
makeState = newState . fmap (RuleStep mempty)

newState :: StepCore l a -> a -> State l a
newState core a = push core (S emptyStack [] [] 0 a)

----------------------------------------------------------------------
-- Parse derivation tree

parseDerivationTree :: State l a -> DerivationTree (Step l a) (State l a)
parseDerivationTree = makeTree $ \state ->
   let xs = firsts state
   in ( any (isReady . fst) xs
      , [ (step, s) | (Result step, s) <- xs ]
      )

firsts :: State l a -> [(Result (Step l a), State l a)]
firsts st =
   case pop st of
      Nothing        -> [(Ready, st)]
      Just (core, s) -> firstsStep core s
 where
   firstsStep core state =
      case core of
         a :*: b   -> firstsStep a (push b state)
         a :|: b   -> chooseFor True a ++ chooseFor False b
         a :%: b   -> firstsStep (coreInterleave a b) state
         a :!%: b  -> firstsStep a (suspend b state)
         Rec i a   -> incrTimer state >>= firstsStep (substCoreVar i core a)
         Var _     -> freeCoreVar "firsts"
         Rule r    -> hasStep r
         Label l a -> firstsStep (coreLabel l a) state
         Atomic a  -> firstsStep a (useAtomic state)
         Not a     -> guard (checkNot a state) >> firsts state
         a :|>: b  -> firstsStep (coreOrElse a b) state
         Many a    -> firstsStep (coreMany a) state
         Repeat a  -> firstsStep (coreRepeat a) state
         Fail      -> []
         Succeed   -> firsts state
    where
      chooseFor b  = flip firstsStep (makeChoice b state)
      hasStep step = [ (Result (head (trace s)), s) | s <- useRule step state ]

-- helper datatype
data Result a = Result a | Ready deriving  Show

instance Functor Result where
   fmap f (Result a) = Result (f a)
   fmap _ Ready      = Ready

isReady :: Result a -> Bool
isReady Ready = True
isReady _     = False

----------------------------------------------------------------------
-- Running the parser

runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

runState :: State l a -> [a]
runState st =
   case pop st of
      Nothing        -> [value st]
      Just (core, s) -> runStep core s
 where
   runStep core state =
      case core of
         a :*: b   -> runStep a (push b state)
         a :|: b   -> runStep a state ++ runStep b state
         a :%: b   -> runStep (coreInterleave a b) state
         a :!%: b  -> runStep a (suspend b state)
         Rec i a   -> incrTimer state >>= runStep (substCoreVar i core a)
         Var _     -> freeCoreVar "runState"
         Rule  r   -> concatMap runState (useRule r (interleave r state))
         Label _ a -> runStep a state
         Atomic a  -> runStep a (useAtomic state)
         Not a     -> guard (checkNot a state) >> runState state
         a :|>: b  -> let xs = runStep a state
                      in if null xs then runStep b state else xs
         Many a    -> runStep (coreMany a) state
         Repeat a  -> runStep (coreRepeat a) state
         Fail      -> []
         Succeed   -> runState state

----------------------------------------------------------------------
-- Replay a parse run

replay :: Monad m => Int -> [Bool] -> Core l a -> m (State l a)
replay n0 bs0 = replayState n0 bs0 . flip makeState noValue
 where
   noValue = error "no value in replay"

   replayState n bs state =
      case pop state of
         _ | n==0       -> return state
         Nothing        -> return state
         Just (core, s) -> replayStep n bs core s

   replayStep n bs core state =
      case core of
         _ | n==0  -> return state
         a :*: b   -> replayStep n bs a (push b state)
         a :|: b   -> case bs of
                        []   -> fail "replay failed"
                        x:xs -> let new = if x then a else b
                                in replayStep n xs new (makeChoice x state)
         a :%: b   -> replayStep n bs (coreInterleave a b) state
         a :!%: b  -> replayStep n bs a (suspend b state)
         Rec i a   -> replayStep n bs (substCoreVar i core a) state
         Var _     -> freeCoreVar "replay"
         Rule r    -> replayState (n-1) bs (traceStep r state)
         Label l a -> replayStep n bs (coreLabel l a) state
         Atomic a  -> replayStep n bs a (useAtomic state)
         Not _     -> replayState n bs state
         a :|>: b  -> replayStep n bs (coreOrElse a b) state
         Many a    -> replayStep n bs (coreMany a) state
         Repeat a  -> replayStep n bs (coreRepeat a) state
         Fail      -> fail "replay failed"
         Succeed   -> replayState n bs state

----------------------------------------------------------------------
-- Core translations

coreLabel :: l -> StepCore l a -> StepCore l a
coreLabel l a = Rule (Enter l) :*: a :*: Rule (Exit l)

coreInterleave :: StepCore l a -> StepCore l a -> StepCore l a
coreInterleave a b = (a :!%: b) :|: (b :!%: a) :|: emptyOnly (a :*: b)
 where
   emptyOnly core =
      case core of
         Rule step | interleaveAfter step -> Fail
         Not _    -> core
         x :|>: y -> emptyOnly x .|. (Not x :*: emptyOnly y)
         Repeat x -> emptyOnly (coreRepeat x)
         x :|: y  -> emptyOnly x .|. emptyOnly y
         x :*: y  -> emptyOnly x .*. emptyOnly y
         x :%: y  -> emptyOnly x .*. emptyOnly y -- no more interleaving
         x :!%: y -> emptyOnly x .*. emptyOnly y -- no more interleaving
         _        -> descend emptyOnly core

----------------------------------------------------------------------
-- State functions

push :: StepCore l a -> State l a -> State l a
push = changeStack . pushStack

suspend :: StepCore l a -> State l a -> State l a
suspend = changeStack . suspendStack

useAtomic :: State l a -> State l a
useAtomic = changeStack interleaveStack

pop :: State l a -> Maybe (StepCore l a, State l a)
pop s = fmap (second f) (popStack (stack s))
 where f new = s {stack = new}

makeChoice :: Bool -> State l a -> State l a
makeChoice b s = s {choices = b : choices s}

checkNot :: StepCore l a -> State l a -> Bool
checkNot core = null . runState . newState core . value

useRule :: Step l a -> State l a -> [State l a]
useRule step state = map resetTimer $
   case step of
      RuleStep _ r -> do
         (a, env) <- applyRule r (value state)
         return $ traceStep (RuleStep env r) state {value = a}
      _ -> [traceStep step state]

traceStep :: Step l a -> State l a -> State l a
traceStep step s = interleave step s {trace = step : trace s}

freeCoreVar :: String -> a
freeCoreVar caller = error $ "Free var in core expression: " ++ caller

incrTimer :: Monad m => State l a -> m (State l a)
incrTimer s
   | timeout s >= 20 = fail "timeout after 20 fixpoints"
   | otherwise       = return (s {timeout = timeout s + 1})

resetTimer :: State l a -> State l a
resetTimer s = s {timeout = 0}

interleaveAfter :: Step l a -> Bool
interleaveAfter (RuleStep _ _) = True
interleaveAfter _              = False

interleave :: Step l a -> State l a -> State l a
interleave step = if interleaveAfter step then useAtomic else id

changeStack :: (Stack l a -> Stack l a) -> State l a -> State l a
changeStack f s = s {stack = f (stack s)}

----------------------------------------------------------------------
-- Stack functions

emptyStack :: Stack l a
emptyStack = Stack [] [] []

pushStack :: StepCore l a -> Stack l a -> Stack l a
pushStack core s = s {active = core : active s}

suspendStack :: StepCore l a -> Stack l a -> Stack l a
suspendStack core s
   | null (active s) = s {suspended = core : suspended s}
   | otherwise = emptyStack {suspended = [core], remainder = combineStack s}

popStack :: Stack l a -> Maybe (StepCore l a, Stack l a)
popStack s =
   case active s of
      x:xs -> Just (x, s {active = xs})
      [] | null (suspended s) -> Nothing
         | otherwise          -> Just (Fail, s)

interleaveStack :: Stack l a -> Stack l a
interleaveStack s = emptyStack {active = combineStack s}

combineStack :: Stack l a -> [StepCore l a]
combineStack s
   | null (suspended s) = active s
   | otherwise = front : remainder s
 where
   actives = foldr (.*.) Succeed (active s)
   front   = foldr (.%.) Succeed (actives:suspended s)