{-# LANGUAGE GADTs, RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
--  $Id$

module Ideas.Encoding.Evaluator
   ( Evaluator(..), evalService
   ) where

import Ideas.Encoding.Encoder
import Ideas.Service.Types

data Evaluator a b c = Evaluator (TypedDecoder a b) (TypedEncoder a c)

evalService :: Options a -> Evaluator a b c -> Service -> b -> IO c
evalService opts f = eval opts f . serviceFunction

eval :: Options a -> Evaluator a b c -> TypedValue (Type a) -> b -> IO c
eval opts f@(Evaluator dec enc) tv@(val ::: tp) b =
   case tp of
      -- handle exceptions
      Const String :|: t ->
         either fail (\a -> eval opts f (a ::: t) b) val
      -- uncurry function if possible
      t1 :-> t2 :-> t3 ->
         eval opts f (uncurry val ::: Pair t1 t2 :-> t3) b
      t1 :-> t2 -> do
         a <- run (dec t1) opts b
         eval opts f (val a ::: t2) b
      -- perform IO
      IO t -> do
         a <- val
         eval opts f (a ::: t) b
      _ ->
         run enc opts tv