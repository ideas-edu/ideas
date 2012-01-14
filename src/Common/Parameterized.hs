{-# LANGUAGE GADTs #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Parameterization
--
-----------------------------------------------------------------------------
module Common.Parameterized 
   ( Parameterized
   , parameter1, parameter2, parameter3, unParam
   ) where

import Common.Binding
import Common.Classes
import Common.Id
import Common.View
import qualified Control.Category as C
import Control.Arrow
import Data.Typeable

data Parameterized b a where
   Pure :: (a -> b) -> Parameterized a b
   Abs  :: Typeable b => Binding b -> (b -> Parameterized c a) -> Parameterized (b, c) a
   App  :: Parameterized (c, b) a -> c -> Parameterized b a
   Iso  :: Isomorphism c b -> Parameterized c a -> Parameterized b a
   Anon :: (b -> Parameterized c a) -> Parameterized (b, c) a

unit :: a -> Parameterized () a
unit = arr . const

instance Functor (Parameterized b) where
   fmap = flip (>>^)

instance C.Category Parameterized where
   id    = arr id
   f . g = comp g f

instance Arrow Parameterized where
   arr = Pure
   first f = Iso swapView $ Anon $ \c -> fmap (\b -> (b, c)) f

instance ArrowChoice Parameterized where
   left f = Iso fstIso $ Anon $ either (app2 (fmap Left f)) (unit . Right)

-- uncurry
join :: Parameterized a (Parameterized b c) -> Parameterized (a, b) c
join (Pure f)  = Anon f
join (Abs a f) = Iso pairLeftIso $ Abs a (join . f)
join (App a b) = App (Iso pairRightIso $ join a) b
join (Iso a b) = Iso (first a) $ join b
join (Anon g)  = Iso pairLeftIso $ Anon (join . g)

app2 :: Parameterized c a -> c -> Parameterized () a
app2 a b = App (Iso (inverse fstIso) a) b

comp :: Parameterized a b -> Parameterized b c -> Parameterized a c
comp (Pure f) x = Iso fstIso $ Anon $ \a -> app2 x (f a)
comp (Abs a f) x = Abs a $ \b -> comp (f b) x
comp (Anon f)  x = Anon $ \b -> comp (f b) x
comp (App a b) x = App (comp a x) b
comp (Iso a b) x = Iso a $ comp b x

toParameterized :: a -> Parameterized () a
toParameterized = unit

param :: (IsId n, Bindable b) => n -> (b -> Parameterized c a) -> Parameterized (b, c) a
param = Abs . makeBinding

parameter1 :: (IsId n1, Bindable b) => n1 -> (b -> a) -> Parameterized b a
parameter1 n1 f = Iso fstIso $ param n1 (toParameterized . f)

parameter2 :: (IsId n1, IsId n2, Bindable b, Bindable c) 
           => n1 -> n2 -> (b -> c -> a) -> Parameterized (b, c) a
parameter2 n1 n2 f = param n1 (parameter1 n2 . f)

parameter3 :: (IsId n1, IsId n2, IsId n3, Bindable b, Bindable c, Bindable d) 
           => n1 -> n2 -> n3 -> (b -> c -> d -> a) -> Parameterized (b, c, d) a
parameter3 n1 n2 n3 f = Iso tripleIso $ param n1 (parameter2 n2 n3 . f)

unParam :: Parameterized b a -> b -> (a, Environment)
unParam = unParamWith mempty

unParamWith :: Environment -> Parameterized b a -> b -> (a, Environment)
unParamWith env f b =
   case f of 
      Pure g  -> (g b, env)
      Abs a g -> let (b1, b2) = b
                     new = insertBinding (setValue b1 a) env
                 in unParamWith new (g b1) b2
      App a c -> unParamWith env a (c, b)
      Iso v a -> unParamWith env a (to v b)
      Anon g  -> let (b1, b2) = b
                 in unParamWith env (g b1) b2
         
fstIso :: Isomorphism (a, ()) a
fstIso = fst <-> (\x -> (x, ()))

pairLeftIso :: Isomorphism (a, (b, c)) ((a, b), c)
pairLeftIso = f <-> g
 where
   f (x, (y, z)) = ((x, y), z)
   g ((x, y), z) = (x, (y, z))

pairRightIso :: Isomorphism ((a, b), c) (a, (b, c))
pairRightIso = inverse pairLeftIso

tripleIso :: Isomorphism (a, (b, c)) (a, b, c)
tripleIso = f <-> g
 where
   f (x, (y, z)) = (x, y, z)
   g (x, y, z)   = (x, (y, z))