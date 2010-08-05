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
mod ule Common.Strategy.Parsing (runCore, stateToStepTree, treeCore, replay, counter, choices, State, value, trace, makeState {-, stateToTree -}) where
  
import Prelude hiding (repeat)
import Common.Classes
import Common.Derivation
import Common.Strategy.Grammar (Step(..))
import Common.Strategy.Core
import Common.Transformation
import Common.Uniplate
import Control.Monad
import qualified Data.IntMap as IM
import Data.Maybe
import Common.Utils (safeHead)

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
   , value       :: a                                -- current value
   }

{-
empty :: State l a -> Bool
empty state = isReadyState state || any p (step state)
 where
   p st = lastIsSkip st && empty st -}

makeState :: Core l a -> a -> State l a
makeState core a = S core [] (Env IM.empty) [] [] 0 False a


runCore :: Core l a -> a -> [a]
runCore core = runState . makeState core

stateToTree :: State l a -> DerivationTree (Maybe (Step l a)) (State l a)
stateToTree state = addBranches list (singleNode state (isReadyState state))
 where
   list = [ (f s, stateToTree s) | s <- step state ]
   f s  = case trace s of
             hd:_ | not (lastIsSkip s) -> Just hd
             _ -> Nothing

smallEmpty :: State l a -> Bool
smallEmpty s = isReadyState s || any p (step s)
 where
   p st = lastIsSkip st && smallEmpty st

smallFinal :: State l a -> Maybe (State l a)
smallFinal s 
   | isReadyState s = Just s
   | lastIsSkip s   = safeHead (catMaybes (map smallFinal (step s)))
   | otherwise      = Nothing

smallStep :: State l a -> [(Step l a, State l a)]
smallStep = concatMap f . step
 where
   f s | lastIsSkip s = smallStep s
       | otherwise    = [(head (trace s), fromMaybe s (smallFinal s))]

stateToStepTree :: State l a -> DerivationTree (Step l a) (State l a)
stateToStepTree state = addBranches list node --  mapSteps fromJust . mergeSteps isJust . stateToTree
 where 
   node = singleNode state (smallEmpty state)
   list = [ (s, stateToStepTree n) | (s, n) <- smallStep state ]

treeCore :: Core l a -> a -> DerivationTree (Rule a) a
treeCore core a = f (mergeSteps p (stateToTree (makeState core a)))
 where
   p (Just (RuleStep _ _)) = True
   p _                     = False
   f = mapSteps (\(Just (RuleStep _ r)) -> r) . fmap value

runState :: State l a -> [a]
runState state = map value $ results $ stateToTree state

isReadyState :: State l a -> Bool
isReadyState state = 
   case grammar state of
      Succeed -> null (stack state)
      _       -> False

step :: State l a -> [State l a]
step = map fst . tinyStep

oneStep :: State l a -> [(State l a, Step l a)]
oneStep = concatMap f . tinyStep
 where
   f (state, Just step) = [(state, step)]
   f (state, Nothing)   = oneStep state

tinyStep :: State l a -> [(State l a, Maybe (Step l a))]
tinyStep state = update $
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
      Rule l r  -> [ (state { grammar = Succeed, value = b }, Just (RuleStep l r))
                   | b <- applyAll r (value state)
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
   update = map $ \(s, mstep) -> (s
      { counter    = counter s + 1
      , trace      = maybe id (:) mstep (trace s)
      , lastIsSkip = isNothing mstep
      }, mstep)

replay :: Monad m => Int -> [Bool] -> State l a -> m (State l a)
replay 0 _  state = return state
replay n bs state =
   case grammar state of
      Rule l r -> 
         replay (n-1) bs state 
            {grammar = Succeed, counter = counter state + 1, trace = RuleStep l r : trace state, lastIsSkip = False}
      Not _ ->
         replay (n-1) bs state
            {grammar = Succeed, counter = counter state + 1, lastIsSkip = True}
      _ -> 
         case (step state, bs) of
            ([s], _)         -> replay (n-1) bs s
            ([s1, s2], c:cs) -> replay (n-1) cs (if c then s1 else s2)
            _                -> fail "replay: invalid step" 

replaceBy :: Core l a -> State l a -> [(State l a, Maybe (Step l a))]
replaceBy core state = return (state { grammar = core }, Nothing)

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