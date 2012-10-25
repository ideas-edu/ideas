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

import Service.Types

evalService :: Monad m => Evaluator m out a -> Service -> m out
evalService f = eval f . serviceFunction

data Evaluator m out a = Evaluator
   { encoder :: Encoder m out a
   , decoder :: Decoder m a
   }

type Encoder m out a = TypedValue a -> m out

data Decoder m a = Decoder { decode :: forall t . Type a t -> m t }

eval :: Monad m => Evaluator m out a -> TypedValue a -> m out
eval f tv@(val ::: tp) =
   case tp of
      t1 :-> t2 -> do
         a <- decode (decoder f) t1
         eval f (val a ::: t2)
      _ ->
         encoder f tv