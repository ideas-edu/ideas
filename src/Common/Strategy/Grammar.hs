module Common.Strategy.Grammar
   ( makeTree 
   ) where

import Common.Derivation
import Common.Strategy.Core
import Common.Strategy.Parsing (Step)
import qualified Common.Strategy.Parsing as Step

----------------------------------------------------------------------
-- Abstract data type

data State l a = S 
   { grammar :: Core l a
   , stack   :: [Either l (Core l a)]
   }

makeTree :: Core l a -> DerivationTree (Step l a) ()
makeTree g = f (S g [])
 where
   f st = addBranches list node
    where
      node = singleNode () (empty st)
      list = [ (step, f rest) | (step, rest) <- firsts st ]

----------------------------------------------------------------------
-- Elementary operations

-- | Tests whether the grammar accepts the empty string
empty :: State l a -> Bool
empty (S g xs)  = all (either (const False) f) (Right g:xs)
 where
   f (s :*: t)   =  f s && f t
   f (s :|: t)   =  f s || f t
   f (Rec _ s)   =  f s
   f (Label _ s) =  f s
   f Succeed     =  True
   f _           =  False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: State l a -> [(Step l a, State l a)]
firsts state =
   case grammar state of
      s :*: t ->  firsts (S s (Right t : stack state))
      s :|: t ->  firsts (state {grammar = s}) ++ 
                  firsts (state {grammar = t})
      Rec i s ->  firsts state {grammar = replaceVar i (Rec i s) s}
      Rule  Nothing a ->  [(Step.RuleStep Nothing a, succeed)]
      Rule  (Just l) a -> let new = Label l (Rule Nothing a)
                           in firsts state {grammar = new}
      Label l s -> [(Step.Enter l, state {grammar = s, stack = Left l : stack state})]
      this -> case stack state of 
                 Right x:xs | empty (S this []) -> firsts (S x xs)
                 Left l:xs  | empty (S this []) -> [(Step.Exit l, state {stack = xs})]
                 _ -> []
 where
   succeed = state {grammar = Succeed}

----------------------------------------------------------------------
-- Local helper functions and instances

replaceVar :: Int -> Core l a -> Core l a -> Core l a
replaceVar i new = rec 
 where
   rec g =
      case g of 
         Var j   | i==j -> new
         Rec j _ | i==j -> g
         s :*: t        -> rec s :*: rec t
         s :|: t        -> rec s :|: rec t
         Label l s      -> Label l (rec s)
         _              -> g
