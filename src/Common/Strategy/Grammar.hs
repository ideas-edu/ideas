module Common.Strategy.Grammar
   ( Grammar(..)
   , empty, firsts 
   ) where

----------------------------------------------------------------------
-- Abstract data type

data Grammar a  =  Grammar a :*: Grammar a 
                |  Grammar a :|: Grammar a 
                |  Rec Int (Grammar a) 
                |  Symbol a | Var Int | Succeed | Fail 

----------------------------------------------------------------------
-- Elementary operations

-- | Tests whether the grammar accepts the empty string
empty :: Grammar a -> Bool
empty (s :*: t)   =  empty s && empty t
empty (s :|: t)   =  empty s || empty t
empty (Rec _ s)   =  empty s
empty Succeed     =  True
empty _           =  False

-- | Returns the firsts set of the grammar, where each symbol is
-- paired with the remaining grammar
firsts :: Grammar a -> [(a, Grammar a)]
firsts (s :*: t)   =  [ (a, s' :*: t) | (a, s') <- firsts s ] ++
                      (if empty s then firsts t else [])
firsts (s :|: t)   =  firsts s ++ firsts t
firsts (Rec i s)   =  firsts (replaceVar i (Rec i s) s)
firsts (Symbol a)  =  [(a, Succeed)]
firsts _           =  []

----------------------------------------------------------------------
-- Local helper functions and instances

replaceVar :: Int -> Grammar a -> Grammar a -> Grammar a
replaceVar i new = rec 
 where
   rec g =
      case g of 
         Var j   | i==j -> new
         Rec j _ | i==j -> g
         s :*: t        -> rec s :*: rec t
         s :|: t        -> rec s :|: rec t
         _              -> g