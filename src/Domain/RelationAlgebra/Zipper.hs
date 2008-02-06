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
module Domain.RelationAlgebra.Zipper where

import Common.Move
import Common.Transformation
import Domain.RelationAlgebra.Formula
            
data Cxt = Top
         | CompL Cxt RelAlg
         | CompR RelAlg Cxt
         | AddL Cxt RelAlg
         | AddR RelAlg Cxt
         | AndL Cxt RelAlg
         | AndR RelAlg Cxt
         | OrL Cxt RelAlg
         | OrR RelAlg Cxt
         | NotD Cxt 
         | InvD Cxt
   deriving (Show, Eq, Ord)

data Loc a = Loc Cxt a
   deriving Show

instance Eq RelAlgInContext where
   x==y = noContext x==noContext y

instance Functor Loc where
   fmap f (Loc c a) = Loc c (f a)

type RelAlgInContext = Loc RelAlg

instance Move RelAlgInContext where
   moveLeft loc@(Loc ctx it) =
      case it of
         a :.:  b -> Just (Loc (CompL ctx b) a)
         a :+:  b -> Just (Loc (AddL  ctx b) a)
         a :&&: b -> Just (Loc (AndL  ctx b) a)
         a :||: b -> Just (Loc (OrL   ctx b) a)
         _        -> Nothing
    
   moveRight loc@(Loc ctx it) =
      case it of
         a :.:  b -> Just (Loc (CompR a ctx) b)
         a :+:  b -> Just (Loc (AddR  a ctx) b)
         a :&&: b -> Just (Loc (AndR  a ctx) b)
         a :||: b -> Just (Loc (OrR   a ctx) b)
         _        -> Nothing

   moveUp loc@(Loc ctx it) =
      case ctx of
         CompL ctx r -> Just (Loc ctx (it :.: r))
         CompR l ctx -> Just (Loc ctx (l  :.: it))
         AddL ctx r  -> Just (Loc ctx (it :+: r))
         AddR l ctx  -> Just (Loc ctx (l  :+: it))
         AndL ctx r  -> Just (Loc ctx (it :&&: r))
         AndR l ctx  -> Just (Loc ctx (l  :&&: it))
         OrL ctx r   -> Just (Loc ctx (it :||: r))
         OrR l ctx   -> Just (Loc ctx (l  :||: it))
         NotD ctx    -> Just (Loc ctx (Not it))
         InvD ctx    -> Just (Loc ctx (Inv it))
         _           -> Nothing
       
   moveDown loc@(Loc ctx it) =
      case it of
         Not a -> Just (Loc (NotD ctx) a)
         Inv a -> Just (Loc (InvD ctx) a)
         _     -> Nothing
 
noContext :: RelAlgInContext -> RelAlg
noContext loc@(Loc ctx it) = 
   let rec f c = noContext (Loc c (f it)) in
   case ctx of
      Top       -> it 
      CompL c l -> rec (:.: l) c
      CompR l c -> rec (l :.:) c  
      AddL c l  -> rec (:+: l) c
      AddR l c  -> rec (l :+:) c
      AndL c l  -> rec (:&&: l) c
      AndR l c  -> rec (l :&&:) c
      OrL c l   -> rec (:||: l) c
      OrR l c   -> rec (l :||:) c
      NotD c    -> rec Not c
      InvD c    -> rec Inv c

inContext :: RelAlg -> RelAlgInContext
inContext x = Loc Top x
     
liftRelAlgRule :: Rule RelAlg -> Rule RelAlgInContext
liftRelAlgRule = liftRule $ LiftPair (\(Loc _ y) -> Just y) (\y (Loc x _) -> Loc x y)