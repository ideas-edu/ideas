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
-- Basic machinery for executing a core strategy expression.
--
-----------------------------------------------------------------------------
module Common.Strategy.Parsing
   ( Step(..)
   , State, makeState, stack, choices, trace, value
   , parseDerivationTree, replay, runCore
   ) where

import Common.Classes
import Common.Derivation
import Common.Strategy.Core
import Common.Transformation
import Common.Uniplate
import Control.Arrow
import Control.Monad

----------------------------------------------------------------------
-- Step data type

data Step l a = Enter l | Exit l | RuleStep (Rule a)
   deriving Show

-- A core expression where the symbols are steps instead of "only" rules
type StepCore l a = GCore l (Step l a)

instance Apply (Step l) where
   applyAll (RuleStep r) = applyAll r
   applyAll _            = return

----------------------------------------------------------------------
-- State data type

data State l a = S
   { stack   :: [StackItem l a]
   , choices :: [Bool]
   , trace   :: [Step l a]
   , timeout :: !Int
   , value   :: a
   }

-- sequential, interleaved, and atomic items
data StackItem l a = SSeq (StepCore l a) | SInt (StepCore l a) | SAtom

makeState :: Core l a -> a -> State l a
makeState = newState . fmap RuleStep

newState :: StepCore l a -> a -> State l a
newState core a = push core (S [] [] [] 0 a)

----------------------------------------------------------------------
-- Parse derivation tree

parseDerivationTree :: State l a -> DerivationTree (Step l a) (State l a)
parseDerivationTree state = addBranches list node
 where
   xs    = firsts state
   empty = not (null [ () | (Ready, _) <- xs ])
   node  = singleNode state empty
   list  = [ (step, parseDerivationTree s) | (Result step, s) <- xs ] 

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
         a :|||: b -> firstsStep (coreInterleave a b) state
         a :||-: b -> firstsStep a (suspend b state)
         Rec i a   -> incrTimer state >>= firstsStep (substCoreVar i core a)
         Var _     -> freeCoreVar "firsts"
         Rule r    -> hasStep r (useRule r state)
         Label l a -> firstsStep (coreLabel l a) state
         Atomic a  -> firstsStep a (pushAtom state)
         Not a     -> guard (checkNot a state) >> firsts state
         a :|>: b  -> firstsStep (coreOrElse a b) state
         Many a    -> firstsStep (coreMany a) state
         Repeat a  -> firstsStep (coreRepeat a) state
         Fail      -> []
         Succeed   -> firsts state
    where
      chooseFor b     = flip firstsStep (makeChoice b state)
      hasStep step xs = [ (Result step, traceStep step s) | s <- xs ]

-- helper datatype
data Result a = Result a | Ready

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
         a :|||: b -> runStep (coreInterleave a b) state
         a :||-: b -> runStep a (suspend b state)
         Rec i a   -> incrTimer state >>= runStep (substCoreVar i core a)
         Var _     -> freeCoreVar "runState"
         Rule  r   -> concatMap runState (useRule r state)
         Label _ a -> runStep a state
         Atomic a  -> runStep a (pushAtom state)
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
         a :|||: b -> replayStep n bs (coreInterleave a b) state
         a :||-: b -> replayStep n bs a (suspend b state)
         Rec i a   -> replayStep n bs (substCoreVar i core a) state
         Var _     -> freeCoreVar "replay"
         Rule r    -> replayState (n-1) bs (traceStep r state)
         Label l a -> replayStep n bs (coreLabel l a) state
         Atomic a  -> replayStep n bs a (pushAtom state)
         Not _     -> replayState n bs state
         a :|>: b  -> replayStep n bs (coreOrElse a b) state
         Many a    -> replayStep n bs (coreMany a) state
         Repeat a  -> replayStep n bs (coreRepeat a) state
         Fail      -> fail "replay failed"
         Succeed   -> replayState n bs state

----------------------------------------------------------------------
-- Local helper functions and instances
   
push :: StepCore l a -> State l a -> State l a
push core s = s {stack = SSeq core : stack s}

pushAtom :: State l a -> State l a
pushAtom s = s {stack = SAtom : stack s}

suspend :: StepCore l a -> State l a -> State l a
suspend core s = s {stack = SInt core : stack s}

pop :: State l a -> Maybe (StepCore l a, State l a)
pop s = case stack s of
           []        -> Nothing
           SSeq x:xs -> Just (x, s {stack = xs})
           SAtom:xs  -> pop s {stack = interleaveItems xs}
           SInt _:_  -> Just (Fail, s {stack = []})
   
makeChoice :: Bool -> State l a -> State l a
makeChoice b s = s {choices = b : choices s}

checkNot :: StepCore l a -> State l a -> Bool
checkNot core = null . runState . newState core . value

useRule :: Step l a -> State l a -> [State l a]
useRule step@(RuleStep _) s = useRule2 step (interleave s)
useRule step s = useRule2 step s

useRule2 :: Step l a -> State l a -> [State l a]
useRule2 step state = 
   [ resetTimer state {value = b} | b <- applyAll step (value state) ]

traceStep :: Step l a -> State l a -> State l a
traceStep step s = s {trace = step : trace s}

freeCoreVar :: String -> a
freeCoreVar caller = error $ "Free var in core expression: " ++ caller

incrTimer :: Monad m => State l a -> m (State l a)
incrTimer s
   | timeout s >= 20 = fail "timeout after 20 fixpoints" 
   | otherwise       = return (s {timeout = timeout s + 1})

resetTimer :: State l a -> State l a
resetTimer s = s {timeout = 0}

coreLabel :: l -> StepCore l a -> StepCore l a
coreLabel l a = Rule (Enter l) :*: a :*: Rule (Exit l)

coreInterleave :: StepCore l a -> StepCore l a -> StepCore l a
coreInterleave a b = (a :||-: b) :|: (b :||-: a) :|: emptyOnly (a :*: b)

emptyOnly :: StepCore l a -> StepCore l a
emptyOnly core =
   case core of
      Rule (RuleStep _) -> Fail
      _ -> descend emptyOnly core

interleave :: State l a -> State l a
interleave s = s {stack = interleaveItems (stack s)}

interleaveItems :: [StackItem l a] -> [StackItem l a]
interleaveItems []  = []
interleaveItems xs = 
   let (seqs, ys) = f xs
       (ints, zs) = g ys
       new = makeInterleave (makeSequence seqs : ints)
   in SSeq new:zs
 where
   f (SSeq x:xs) = first (x:) (f xs)
   f xs          = ([], xs)
   
   g (SInt x:xs) = first (x:) (g xs)
   g xs          = ([], xs)
   
   makeSequence = foldr op Succeed
    where
      op Succeed b = b
      op a Succeed = a
      op a b = a :*: b
   makeInterleave = foldr op Succeed
    where
      op Succeed b = b
      op a Succeed = a
      op a b = a :|||: b