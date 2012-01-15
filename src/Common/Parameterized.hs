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
import Common.Id
import qualified Control.Category as C
import Control.Arrow
import Data.Typeable

data Parameterized b a where
   Pure :: (a -> b) -> Parameterized a b
   Abs  :: Typeable b => Binding b -> (b -> Parameterized c a) -> Parameterized (b, c) a
   Comp :: Parameterized a b -> Parameterized b c -> Parameterized a c
   AppA :: Parameterized (Parameterized a b, a) b
   Loop :: Parameterized (a, c) (b, c) -> Parameterized a b
   Star :: Parameterized a c -> Parameterized b d -> Parameterized (a, b) (c, d)
   Bind :: Typeable a => Binding a -> Parameterized a a

bind :: (IsId n, Bindable a) => n -> Parameterized a a
bind = Bind . makeBinding

join :: Parameterized a (Parameterized b c) -> Parameterized (a, b) c
join f = first f >>> AppA

instance Functor (Parameterized b) where
   fmap = flip (>>^)

instance C.Category Parameterized where
   id    = arr id
   f . g = Comp g f

instance Arrow Parameterized where
   arr = Pure
   (***) = Star
   first f = Star f C.id
   second f = Star C.id f

instance ArrowChoice Parameterized where
   left = leftApp

instance ArrowApply Parameterized where
   app = AppA

instance ArrowLoop Parameterized where
   loop = Loop

parameter1 :: (IsId n1, Bindable b) => n1 -> (b -> a) -> Parameterized b a
parameter1 n1 f = bind n1 >>> arr f

parameter2 :: (IsId n1, IsId n2, Bindable b, Bindable c) 
           => n1 -> n2 -> (b -> c -> a) -> Parameterized (b, c) a
parameter2 n1 n2 f = bind n1 *** bind n2 >>> arr (uncurry f)

parameter3 :: (IsId n1, IsId n2, IsId n3, Bindable b, Bindable c, Bindable d)
           => n1 -> n2 -> n3 -> (b -> c -> d -> a) -> Parameterized (b, c, d) a
parameter3 n1 n2 n3 f = 
   (\(a, b, c) -> (a, (b, c))) ^>> 
   join (fmap (parameter2 n2 n3) (parameter1 n1 f))

unParam :: Parameterized b a -> b -> (a, Environment)
unParam f = unParamWith mempty f

unParamWith :: Environment -> Parameterized b a -> b -> (a, Environment)
unParamWith env f b =
   case f of 
      Pure g  -> (g b, env)
      Abs a g -> let (b1, b2) = b
                     new = insertBinding (setValue b1 a) env
                 in unParamWith new (g b1) b2
      Comp f g -> let (a, new) = unParamWith env f b
                  in unParamWith new g a
      AppA     -> let (f, a) = b
                  in unParamWith env f a
      Loop g   -> let ((a, c), new) = unParamWith env g (b, c)
                  in (a, new)
      Star f g -> let (a, c) = b
                      (x, new1) = unParamWith env f a
                      (y, new2) = unParamWith new1 g c
                  in ((x, y), new2)
      Bind a -> (b, insertBinding (setValue b a) env)