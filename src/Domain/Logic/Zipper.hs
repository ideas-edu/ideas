{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Logic.Zipper where

import Common.Move
import Common.Transformation
import Domain.Logic.Formula

data Cxt = Top
         | ImplL Cxt Logic
         | ImplR Logic Cxt
         | EquivL Cxt Logic
         | EquivR Logic Cxt
         | AndL Cxt Logic
         | AndR Logic Cxt
         | OrL Cxt Logic
         | OrR Logic Cxt
         | NotD Cxt 
   deriving (Show, Eq, Ord)

data Loc a = Loc Cxt a
   deriving Show

instance Eq LogicInContext where
   x==y = noContext x==noContext y

instance Functor Loc where
   fmap f (Loc c a) = Loc c (f a)

type LogicInContext = Loc Logic

instance Move LogicInContext where
   moveLeft loc@(Loc ctx it) =
      case it of
         a :->:  b -> Just (Loc (ImplL  ctx b) a)
         a :<->: b -> Just (Loc (EquivL ctx b) a)
         a :&&:  b -> Just (Loc (AndL   ctx b) a)
         a :||:  b -> Just (Loc (OrL    ctx b) a)
         _         -> Nothing
      
   moveRight loc@(Loc ctx it) =
      case it of
         a :->:  b -> Just (Loc (ImplR  a ctx) b)
         a :<->: b -> Just (Loc (EquivR a ctx) b)
         a :&&:  b -> Just (Loc (AndR   a ctx) b)
         a :||:  b -> Just (Loc (OrR    a ctx) b)
         _         -> Nothing

   moveUp loc@(Loc ctx it) =
      case ctx of
         ImplL ctx r  -> Just (Loc ctx (it :->: r))
         ImplR l ctx  -> Just (Loc ctx (l :->: it))
         EquivL ctx r -> Just (Loc ctx (it :<->: r))
         EquivR l ctx -> Just (Loc ctx (l :<->: it))
         AndL ctx r   -> Just (Loc ctx (it :&&: r))
         AndR l ctx   -> Just (Loc ctx (l :&&: it))
         OrL ctx r    -> Just (Loc ctx (it :||: r))
         OrR l ctx    -> Just (Loc ctx (l :||: it))
         NotD ctx     -> Just (Loc ctx (Not (it)))
         _            -> Nothing
         
   moveDown loc@(Loc ctx it) =
      case it of
         Not a -> Just (Loc (NotD (ctx)) a)
         _     -> Nothing

instance Uniplate Logic where
   uniplate p =
      case p of 
         p :->: q  -> ([p, q], \[a, b] -> a :->:  b)
         p :<->: q -> ([p, q], \[a, b] -> a :<->: b)
         p :&&: q  -> ([p, q], \[a, b] -> a :&&:  b)
         p :||: q  -> ([p, q], \[a, b] -> a :||:  b)
         Not p     -> ([p], \[a] -> Not a)
         _         -> ([], \[] -> p)
         
noContext :: LogicInContext -> Logic
noContext loc@(Loc ctx it) = 
   let rec f c = noContext (Loc c (f it)) in
   case ctx of
      Top       -> it 
      ImplL c l  -> rec (:->: l) c
      ImplR l c  -> rec (l :->:) c  
      EquivL c l -> rec (:<->: l) c
      EquivR l c -> rec (l :<->:) c
      AndL c l   -> rec (:&&: l) c
      AndR l c   -> rec (l :&&:) c
      OrL c l    -> rec (:||: l) c
      OrR l c    -> rec (l :||:) c
      NotD c     -> rec Not c

inContext :: Logic -> LogicInContext
inContext x = Loc Top x
     
liftLogicRule :: Rule Logic -> Rule LogicInContext
liftLogicRule = liftRule $ LiftPair (\(Loc _ y) -> Just y) (\y (Loc x _) -> Loc x y)