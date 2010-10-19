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
   , parseDerivationTree, replay, runCore, runCoreWith
   ) where

import Common.Classes
import Common.Derivation
import Common.Strategy.Core
import Common.Transformation
import Control.Monad

----------------------------------------------------------------------
-- Step data type

data Step l a = Enter l | Exit l | RuleStep (Rule a)
   deriving Show

instance Apply (Step l) where
   applyAll (RuleStep r) = applyAll r
   applyAll _            = return

instance Apply (Core l) where
   applyAll = runCore

----------------------------------------------------------------------
-- State data type

data State l a = S
   { stack       :: [Either l (Core l a)]
   , choices     :: [Bool]
   , trace       :: [Step l a]
   , value       :: a
   }

makeState :: Core l a -> a -> State l a
makeState core a = push core (S [] [] [] a)

----------------------------------------------------------------------
-- Parse derivation tree

parseDerivationTree :: State l a -> DerivationTree (Step l a) (State l a)
parseDerivationTree state = addBranches list node
 where
   results = firsts state
   empty   = not (null [ () | (Ready, _) <- results ])
   node    = singleNode state empty
   list    = [ (step, parseDerivationTree s) | (Result step, s) <- results ] 

firsts :: State l a -> [(Result (Step l a), State l a)]
firsts state =
   case pop state of 
      Nothing              -> [(Ready, state)]
      Just (Left l, s)     -> [(Result (Exit l), traceExit l s)]
      Just (Right core, s) -> firstsStep core s
 where
   firstsStep core state =
      case core of
         a :*: b   -> firstsStep a (push b state)
         a :|: b   -> chooseFor True a ++ chooseFor False b
         Rec i a   -> firstsStep (substCoreVar i core a) state
         Var _     -> freeCoreVar "firsts"
         Rule r    -> hasStep (RuleStep r) (useRule r state)
         Label l a -> hasStep (Enter l) [push a (pushExit l state)]
         Not a     -> guard (checkNot a state) >> firsts state
         a :|>: b  -> firstsStep (coreOrElse a b) state
         Many a    -> firstsStep (coreMany a) state
         Repeat a  -> firstsStep (coreRepeat a) state
         Fail      -> []
         Succeed   -> firsts state
    where
      chooseFor b core = firstsStep core (makeChoice b state)
      hasStep step xs  = [ (Result step, traceStep step s) | s <- xs ]

-- helper datatype
data Result a = Result a | Ready

----------------------------------------------------------------------
-- Running the parser

runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

runCoreWith :: CoreEnv l a -> Core l a -> a -> [a]
runCoreWith env = runCore . substCoreEnv env

runState :: State l a -> [a]
runState state =
   case pop state of
      Nothing              -> [value state]
      Just (Left _, s)     -> runState s
      Just (Right core, s) -> runStep core s
 where
   runStep core state = 
      case core of
         a :*: b   -> runStep a (push b state)
         a :|: b   -> runStep a state ++ runStep b state
         Rec i a   -> runStep (substCoreVar i core a) state
         Var _     -> freeCoreVar "runState"
         Rule  r   -> concatMap runState (useRule r state)
         Label _ a -> runStep a state
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
replay n bs core = replayState n bs (makeState core noValue)
 where
   noValue = error "no value in replay"
 
   replayState n bs state = 
      case pop state of
         _ | n==0             -> return state
         Nothing              -> return state
         Just (Left l, s)     -> replayState (n-1) bs (traceExit l s)
         Just (Right core, s) -> replayStep n bs core s
             
   replayStep n bs core state =
      case core of
         _ | n==0  -> return state
         a :*: b   -> replayStep n bs a (push b state)
         a :|: b   -> case bs of
                        []   -> fail "replay failed"
                        x:xs -> let new = if x then a else b
                                in replayStep n xs new (makeChoice x state)
         Rec i a   -> replayStep n bs (substCoreVar i core a) state
         Var _     -> freeCoreVar "replay"
         Rule r    -> replayState (n-1) bs (traceRule r state)
         Label l a -> replayStep (n-1) bs a (pushExit l (traceEnter l state))
         Not _     -> replayState n bs state
         a :|>: b  -> replayStep n bs (coreOrElse a b) state
         Many a    -> replayStep n bs (coreMany a) state
         Repeat a  -> replayStep n bs (coreRepeat a) state
         Fail      -> fail "replay failed"
         Succeed   -> replayState n bs state

----------------------------------------------------------------------
-- Local helper functions and instances
   
push :: Core l a -> State l a -> State l a
push core s = s {stack = Right core : stack s}

pushExit :: l -> State l a -> State l a
pushExit l s = s {stack = Left l : stack s}

pop :: State l a -> Maybe (Either l (Core l a), State l a)
pop s = case stack s of
           []   -> Nothing
           x:xs -> Just (x, s {stack = xs})
   
makeChoice :: Bool -> State l a -> State l a
makeChoice b s = s {choices = b : choices s}

checkNot :: Core l a -> State l a -> Bool
checkNot core = null . runCore core . value

useRule :: Rule a -> State l a -> [State l a]
useRule r state = [ state {value = b} | b <- applyAll r (value state) ]

traceEnter, traceExit :: l -> State l a -> State l a
traceEnter = traceStep . Enter
traceExit  = traceStep . Exit

traceRule :: Rule a -> State l a -> State l a
traceRule = traceStep . RuleStep

traceStep :: Step l a -> State l a -> State l a
traceStep step s = s {trace = step : trace s}

substCoreVar :: Int -> Core l a -> Core l a -> Core l a
substCoreVar i a = substCoreEnv (insertCoreEnv i a emptyCoreEnv)

freeCoreVar :: String -> a
freeCoreVar caller = error $ "Free var in core expression: " ++ caller