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
-----------------------------------------------------------------------------
module Common.Strategy.Parsing (runCore, treeCore, Step(..)) where
  
import Prelude hiding (repeat)
import Common.Classes
import Common.Derivation
import Common.Strategy.Core
import Common.Transformation
import Common.Uniplate
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe

newtype Env l a = Env (IM.IntMap (Env l a, Core l a))
  
addToEnv :: Int -> Core l a -> Env l a -> Env l a
addToEnv n a env@(Env m) = Env (IM.insert n (env, Rec n a) m)
  
findInEnv :: Int -> Env l a -> Maybe (Env l a, Core l a)
findInEnv n (Env m) = IM.lookup n m

data State l a = S 
   { grammar     :: Core l a                         -- current core strategy
   , stack       :: [Either l (Core l a, Env l a)]   -- exit labels/strategies
   , environment :: Env l a                          -- env for fixpoints
   , trace       :: [Step l a]                       -- steps so far
   , choices     :: [Bool]                           -- choices made
   , counter     :: !Int                             -- number of steps
   , lastIsSkip  :: Bool                             -- last step was without value
   , value       :: Maybe a                          -- current value (if one)
   }

data Step l a = Enter l | Exit l | RuleStep (Maybe l) (Rule a)
   deriving Show

instance Apply (Step l) where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return

empty :: State l a -> Bool
empty state = isReadyState state || any p (step state)
 where
   p st = lastIsSkip st && empty st

makeState :: Core l a -> a -> State l a
makeState core a = S core [] (Env IM.empty) [] [] 0 False (Just a)

instance Apply (Core l) where
   applyAll = runCore

runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

stateToTree :: State l a -> DerivationTree (Maybe (Step l a)) (State l a)
stateToTree state = addBranches list (singleNode state (isReadyState state))
 where
   list = [ (f s, stateToTree s) | s <- step state ]
   f s  = case trace s of
             hd:_ | not (lastIsSkip s) -> Just hd
             _ -> Nothing

treeCore :: Core l a -> a -> DerivationTree (Rule a) a
treeCore core a = f (mergeSteps p (stateToTree (makeState core a)))
 where
   p (Just (RuleStep _ _)) = True
   p _                     = False
   f = mapSteps (\(Just (RuleStep _ r)) -> r) . fmap (fromJust . value)

runState :: State l a -> [a]
runState state = catMaybes $ map value $ results $ stateToTree state

isReadyState :: State l a -> Bool
isReadyState state = 
   case grammar state of
      Succeed -> null (stack state)
      _       -> False

step :: State l a -> [State l a]
step state = update $
   case grammar state of
      p :*: q   -> replaceBy p (pushCoreStack q state)
      p :|: q   -> replaceBy p (choose True  state) ++
                   replaceBy q (choose False state)
      Succeed   -> case stack state of
                      []              -> []
                      Left l:xs       -> return (state { stack = xs }, Just (Exit l))
                      Right (x, e):xs -> replaceBy x (setEnvironment e (state {stack = xs }))
      Fail      -> []
      Label l p -> return (state { grammar = p }, Just (Enter l))
      Rule l r  -> [ (state { grammar = Succeed, value = Just b }, Just (RuleStep l r))
                   | b <- maybe [] (applyAll r) (value state)
                   ]
      Not p     -> do guard $ null $ runState $ 
                         state { grammar = noLabels p, stack = [] }
                      replaceBy Succeed state
      Rec n p   -> replaceBy p (changeEnvironment (addToEnv n p) state)
      Var n     -> case findInEnv n (environment state) of
                      Just (e, p) -> replaceBy p (setEnvironment e state)
                      Nothing     -> error "Free var"
      p :|>: q  -> replaceBy (orElse p q) state
      Many p    -> replaceBy (many p) state
      Repeat p  -> replaceBy (repeat p) state
 where
   update = map $ \(s, mstep) -> s
      { counter    = counter s + 1
      , trace      = maybe id (:) mstep (trace s)
      , lastIsSkip = isNothing mstep
      }

replay :: Monad m => Int -> [Bool] -> State l a -> m (State l a)
replay 0 bs state 
   | null bs   = return state
   | otherwise = fail "replay: choices left"
replay n bs state =
   case (step state, bs) of
      ([s], _)         -> replay (n-1) bs s
      ([s1, s2], c:cs) -> replay (n-1) cs (if c then s1 else s2)
      _                -> fail "replay: invalid step" 

replaceBy :: Core l a -> State l a -> [(State l a, Maybe (Step l a))]
replaceBy a state = return (state { grammar = a }, Nothing)

changeEnvironment :: (Env l a -> Env l a) -> State l a -> State l a
changeEnvironment f state = 
   state { environment = f (environment state) } 

setEnvironment :: Env l a -> State l a -> State l a
setEnvironment a = changeEnvironment (\_ -> a)

pushCoreStack :: Core l a -> State l a -> State l a
pushCoreStack a state = 
   state { stack = (Right (a, environment state)) : stack state }

choose :: Bool -> State l a -> State l a
choose b state = state { choices = b : choices state }

many :: Core l a -> Core l a
many p = Rec n (Succeed :|: (p :*: Var n))
 where n = nextVar p
 
repeat :: Core l a -> Core l a
repeat p = many p :*: Not (noLabels p) 

orElse :: Core l a -> Core l a -> Core l a
orElse a b = a :|: (Not (noLabels a) :*: b) 

nextVar :: Core l a -> Int
nextVar p
   | null xs   = 0
   | otherwise = maximum xs + 1
 where xs = [ n | Rec n _ <- universe p ]