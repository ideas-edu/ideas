module Common.Strategy.Grammar
   ( Step(..), makeTree, runCore, runCoreWith
   ) where

import Common.Classes
import Common.Derivation
import Common.Strategy.Core hiding (emptyEnv)
import Common.Transformation
import qualified Data.IntMap as IM

data Step l a = Enter l | Exit l | RuleStep (Maybe l) (Rule a)
   deriving Show

instance Apply (Step l) where
   applyAll (RuleStep _ r) = applyAll r
   applyAll _              = return


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
empty (S g xs env)  = all (either (const False) (uncurry (flip f))) (Right (g, env):xs)
 where
   f env (s :*: t)   =  f env s && f env t
   f env (s :|: t)   =  f env s || f env t
   f env (Rec i s)   =  f (deleteEnv i env) s
   f env (Var i)     =  maybe False (\(e,a) -> f e a) (findInEnv i env)
   f env (Label _ s) =  f env s
   f _ Succeed       =  True
   f _ _             =  False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: State l a -> [(Step l a, State l a)]
firsts state =
   case grammar state of
      s :*: t ->  firsts state {grammar = s, stack = Right (t, environment state) : stack state}
      s :|: t ->  firsts (state {grammar = s}) ++ 
                  firsts (state {grammar = t})
      Rec i s -> firsts state {grammar = s, environment = addToEnv i s (environment state) }
      Var i -> case findInEnv i (environment state) of
                  Just (e, s)  -> firsts (state {grammar = s, environment = e})
                  Nothing -> error "free var in core expression"
      Rule  Nothing a ->  [(RuleStep Nothing a, succeed)]
      Rule  (Just l) a -> let new = Label l (Rule Nothing a)
                           in firsts state {grammar = new}
      Label l s -> [(Enter l, state {grammar = s, stack = Left l : stack state})]
      Not s -> firsts state {grammar = Rule Nothing (notRule (environment state) (noLabels s))}
      s :|>: t -> firsts state {grammar = s :|: (Not (noLabels s) :*: t)}
      Many s   -> firsts state {grammar = coreMany s}
      Repeat s -> firsts state {grammar = coreRepeat s}
      _ -> case stack state of 
              Right (x,e):xs | currentIsEmpty -> firsts state {grammar = x, stack = xs, environment = e}
              Left l:xs  | currentIsEmpty -> [(Exit l, state {stack = xs})]
              _ -> []
 where
   succeed = state {grammar = Succeed}
   currentIsEmpty = empty state {stack = []}

notRule :: Env l a -> Core l a -> Rule a
notRule env core = checkRule $ null . runCoreWith env core

instance Apply (Core l) where
   applyAll = runCore

treeCore :: Core l a -> a -> DerivationTree (Rule a) a
treeCore = treeCoreWith emptyEnv

treeCoreWith :: Env l a -> Core l a -> a -> DerivationTree (Rule a) a
treeCoreWith env core = treeState (S core [] env)

treeState :: State l a -> a -> DerivationTree (Rule a) a
treeState state a = addBranches (make state) node
 where
   node = singleNode a (empty state)
   make = concatMap f . firsts
   f (Enter _, s)      = make s
   f (RuleStep _ r, s) = [ (r, treeState s b) | b <- applyAll r a ]
   f (Exit _, s)       = make s

runCore :: Core l a -> a -> [a]
runCore = runCoreWith emptyEnv

runCoreWith :: Env l a -> Core l a -> a -> [a]
runCoreWith env core = runState (S core [] env) -- results . treeCoreWith env core

runState :: State l a -> a -> [a]
runState state a = 
   (if empty state then (a:) else id) 
   (concatMap f (firsts state))
 where
   f (Enter _,      s) = runState s a
   f (RuleStep _ r, s) = applyAll r a >>= runState s
   f (Exit _,       s) = runState s a

----------------------------------------------------------------------
-- Local helper functions and instances

newtype Env l a = Env (IM.IntMap (Env l a, Core l a))

emptyEnv :: Env l a
emptyEnv = Env IM.empty
  
addToEnv :: Int -> Core l a -> Env l a -> Env l a
addToEnv n a env@(Env m) = Env (IM.insert n (env, a) m)
 
deleteEnv :: Int -> Env l a -> Env l a
deleteEnv n (Env m) = Env (IM.delete n m)
  
findInEnv :: Int -> Env l a -> Maybe (Env l a, Core l a)
findInEnv n (Env m) = do
   (e, a) <- IM.lookup n m
   return (e, Rec n a)