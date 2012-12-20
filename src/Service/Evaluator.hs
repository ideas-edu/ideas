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
module Service.Evaluator where

import Common.View
import Control.Monad
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Map as M
import Service.Types

evalService :: Monad m => Evaluator (Const b) m a -> Service -> m a
evalService f = eval f . serviceFunction

data Evaluator f m a = Evaluator
   { encoder :: Encoder (TypeRep f) m a
   , decoder :: Decoder (TypeRep f) m
   }

type Encoder    f m a = TypedValue f -> m a
type EncoderMap f m a = M.Map String (Encoder (TypeRep f) m a)

encodeWith :: (Monad m, Monoid a) 
           => EncoderMap f m a -> Encoder f m a -> Encoder (TypeRep f) m a
encodeWith m enc = rec
 where
   rec (val ::: tp) =
      case tp of
         _ :-> _    -> fail "encodeType: function"
         t1 :|: t2  -> case val of
                          Left a  -> rec (a ::: t1)
                          Right a -> rec (a ::: t2) 
         Pair t1 t2 -> liftM2 mappend
                          (rec (fst val ::: t1))
                          (rec (snd val ::: t2))
         List t     -> liftM mconcat (mapM (rec . (::: t)) val)
         Tree t     -> liftM F.fold (T.mapM (rec . (::: t)) val)
         Unit       -> return mempty
         Tag s t    -> M.findWithDefault rec s m (val ::: t)
         Iso v t    -> rec (to v val ::: t)
         Const ctp  -> enc (val ::: ctp)

data Decoder f m = Decoder { decode :: forall t . f t -> m t }

eval :: Monad m => Evaluator f m a -> TypedValue (TypeRep f) -> m a
eval f tv@(val ::: tp) =
   case tp of
      t1 :-> t2 -> do
         a <- decode (decoder f) t1
         eval f (val a ::: t2)
      _ ->
         encoder f tv