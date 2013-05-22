{-# LANGUAGE GADTs, Rank2Types #-}
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
-----------------------------------------------------------------------------
module Ideas.Service.Evaluator where

import Ideas.Common.View
import Data.Monoid
import qualified Data.Foldable as F
import Ideas.Service.Types
import Data.Function

evalService :: Monad m => Evaluator (Const b) m a -> Service -> m a
evalService f = eval f . serviceFunction

data Evaluator f m a = Evaluator
   { encoder :: Encoder (TypeRep f) (m a)
   , decoder :: Decoder (TypeRep f) m
   }

type Encoder f a = TypedValue f -> a

type Fix a = a -> a

encodeTypeRep :: Monoid a => Encoder f a -> Encoder (TypeRep f) a
encodeTypeRep = fix . encodeTypeRepFix

encodeTypeRepFix :: Monoid a => Encoder f a -> Fix (Encoder (TypeRep f) a)
encodeTypeRepFix enc rec (val ::: tp) =
   case tp of
      _ :-> _    -> mempty
      t1 :|: t2  -> case val of
                       Left a  -> rec (a ::: t1)
                       Right a -> rec (a ::: t2)
      Pair t1 t2 -> rec (fst val ::: t1) <> rec (snd val ::: t2)
      List t     -> mconcat (map (rec . (::: t)) val)
      Tree t     -> F.fold (fmap (rec . (::: t)) val)
      Unit       -> mempty
      Tag _ t    -> rec (val ::: t)
      Iso v t    -> rec (to v val ::: t)
      Const ctp  -> enc (val ::: ctp)
      
encodeWith :: (Monad m, Typed a t) => (t -> m b) -> Encoder (Type a) (m b)
encodeWith enc (val ::: tp) = 
   case equal tp typed of 
      Just f  -> enc (f val)
      Nothing -> fail "encoding failed"

data Decoder f m = Decoder { decode :: forall t . f t -> m t }

eval :: Monad m => Evaluator f m a -> TypedValue (TypeRep f) -> m a
eval f tv@(val ::: tp) =
   case tp of
      t1 :-> t2 -> do
         a <- decode (decoder f) t1
         eval f (val a ::: t2)
      _ ->
         encoder f tv