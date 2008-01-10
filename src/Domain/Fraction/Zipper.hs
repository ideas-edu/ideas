{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex.gerdes@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (todo)
--
-----------------------------------------------------------------------------
module Domain.Fraction.Zipper where

import Common.Move
import Common.Transformation
import Domain.Fraction.Frac

data Cxt = Top
         | MulL Cxt (Frac)
         | MulR (Frac) Cxt
         | DivL Cxt (Frac)
         | DivR (Frac) Cxt
         | AddL Cxt (Frac)
         | AddR (Frac) Cxt
         | SubL Cxt (Frac)
         | SubR (Frac) Cxt
   deriving (Show, Eq, Ord)

data Loc a = Loc Cxt a
   deriving Show

instance Eq FracInContext where
   x==y = noContext x==noContext y

instance Functor Loc where
   fmap f (Loc c a) = Loc c (f a)

type FracInContext = Loc (Frac)

instance Move FracInContext where
   moveLeft loc@(Loc ctx it) =
      case it of
         a :*:  b -> Just (Loc (MulL  ctx b) a)
         a :/:  b -> Just (Loc (DivL  ctx b) a)
         a :+:  b -> Just (Loc (AddL  ctx b) a)
         a :-:  b -> Just (Loc (SubL  ctx b) a)
         _        -> Nothing
      
   moveRight loc@(Loc ctx it) =
      case it of
         a :*:  b -> Just (Loc (MulR  a ctx) b)
         a :/:  b -> Just (Loc (DivR  a ctx) b)
         a :+:  b -> Just (Loc (AddR  a ctx) b)
         a :-:  b -> Just (Loc (SubR  a ctx) b)
         _        -> Nothing

   moveUp loc@(Loc ctx it) =
      case ctx of
         MulL ctx r -> Just (Loc ctx (it :*: r))
         MulR l ctx -> Just (Loc ctx (l :*: it))
         DivL ctx r -> Just (Loc ctx (it :/: r))
         DivR l ctx -> Just (Loc ctx (l :/: it))
         AddL ctx r -> Just (Loc ctx (it :+: r))
         AddR l ctx -> Just (Loc ctx (l :+: it))
         SubL ctx r -> Just (Loc ctx (it :-: r))
         SubR l ctx -> Just (Loc ctx (l :-: it))
         _          -> Nothing
         
   moveDown loc@(Loc ctx it) =
      case it of
--         Not a -> Just (Loc (NotD (ctx)) a)
         _     -> Nothing
         
noContext :: FracInContext -> (Frac)
noContext loc@(Loc ctx it) = 
   let rec f c = noContext (Loc c (f it)) in
   case ctx of
      Top      -> it 
      MulL c l -> rec (:*: l) c
      MulR l c -> rec (l :*:) c  
      DivL c l -> rec (:/: l) c
      DivR l c -> rec (l :/:) c
      AddL c l -> rec (:+: l) c
      AddR l c -> rec (l :+:) c
      SubL c l -> rec (:-: l) c
      SubR l c -> rec (l :-:) c

inContext :: (Frac) -> FracInContext
inContext x = Loc Top x
     
liftFracRule :: Rule (Frac) -> Rule FracInContext
liftFracRule = liftRule $ LiftPair (\(Loc _ y) -> Just y) (\y (Loc x _) -> Loc x y)
