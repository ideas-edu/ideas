{-# LANGUAGE GADTs, RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Ideas.Encoding.Evaluator
   ( EncoderState, simpleEncoder, maybeEncoder, eitherEncoder
   , encoderFor, encoderStateFor, encodeTyped
   , runEncoderState, runEncoderStateM, (//)
   , getState, withState
     -- re-export
   , pure, (<$>), (<**>)
   , module Data.Monoid, liftA2
     -- old
   , Evaluator(..), evalService
   ) where

import Control.Applicative hiding (Const)
import Control.Arrow
import Control.Monad
import Data.List
import Data.Monoid
import Ideas.Common.Classes
import Ideas.Service.Types
import Ideas.Text.XML
import qualified Control.Category as C

newtype EncoderState st a b = Enc (st -> a -> Either [String] b)

instance C.Category (EncoderState st) where
   id = Enc $ \_ -> Right
   Enc f . Enc g = Enc $ \st -> either Left (f st) . g st

instance Arrow (EncoderState st) where
   arr f = Enc $ \_ -> Right . f
   first  (Enc f) = Enc $ \st (a, c) -> fmap (\b -> (b, c)) (f st a)
   second (Enc f) = Enc $ \st (a, b) -> fmap (\c -> (a, c)) (f st b)
   Enc f *** Enc g = Enc $ \st (a, b) ->
      case (f st a, g st b) of
         (Right c, Right d) -> Right (c, d)
         (Left err, _)      -> Left err
         (_, Left err)      -> Left err

instance ArrowZero (EncoderState st) where
   zeroArrow = Enc $ \_ _ -> Left []

instance ArrowPlus (EncoderState st) where
   Enc f <+> Enc g = Enc $ \st a ->
      case (f st a, g st a) of
         (Right b, _      ) -> Right b
         (_,       Right b) -> Right b
         (Left e1, Left e2) -> Left (e1 ++ e2)

instance ArrowChoice (EncoderState st) where
   left  (Enc f) = Enc $ \st -> either (fmap Left . f st) (Right . Right)
   right (Enc f) = Enc $ \st -> either (Right . Left) (fmap Right . f st)
   Enc f +++ Enc g = Enc $ \st -> either (fmap Left . f st) (fmap Right . g st)

instance ArrowApply (EncoderState st) where
   app = Enc $ \st (Enc f, a) -> f st a

instance Functor (EncoderState st a) where
   fmap = liftA

instance Applicative (EncoderState st a) where
   pure    = arr . const
   f <*> g = f &&& g >>> arr (uncurry ($))

instance Monoid b => Monoid (EncoderState st a b) where
   mempty  = pure mempty
   mappend = liftA2 (<>)

instance Monad (EncoderState st a) where
   return = pure
   fail s = Enc $ \_ _ -> Left [ s | not (null s) ]
   Enc f >>= g = Enc $ \st a ->
      case f st a of
         Left err -> Left err
         Right b  -> let Enc h = g b in h st a

instance MonadPlus (EncoderState st a) where
   mzero = zeroArrow
   mplus = (<+>)

instance BuildXML b => BuildXML (EncoderState st a b) where
   n .=. s   = return (n .=. s)
   unescaped = return . unescaped
   builder   = return . builder
   tag       = liftM . tag

getState :: EncoderState st a st
getState = Enc $ const . Right

withState :: (st -> b) -> EncoderState st a b
withState f = liftM f getState

runEncoderState :: EncoderState st a b -> st -> a -> Either String b
runEncoderState (Enc f) st = mapFirst (concat . intersperse ", ") . f st

---

simpleEncoder :: (a -> b) -> EncoderState st a b
simpleEncoder = arr

maybeEncoder :: (a -> Maybe b) -> EncoderState st a b
maybeEncoder f = C.id >>= maybe mzero return . f

eitherEncoder :: (a -> Either String b) -> EncoderState st a b
eitherEncoder f = C.id >>= either fail return . f

encoderFor :: (a -> EncoderState st a b) -> EncoderState st a b
encoderFor = encoderStateFor . const

encoderStateFor :: (st -> a -> EncoderState st a b) -> EncoderState st a b
encoderStateFor f = do
   st <- getState
   a  <- C.id
   f st a

runEncoderStateM :: Monad m => EncoderState st a b -> st -> a -> m b
runEncoderStateM f st = either fail return . runEncoderState f st

encodeTyped :: Typed a t => EncoderState st t b -> EncoderState st (TypedValue (Type a)) b
encodeTyped enc = fromTyped >>> enc

infixl 8 //

(//) :: EncoderState st a c -> a -> EncoderState st b c
f // a = arr (const a) >>> f

----

fromTyped :: Typed a t => EncoderState st (TypedValue (Type a)) t
fromTyped = maybeEncoder $ \(val ::: tp) -> fmap ($ val) (equal tp typed)

-------------------------------------------------------------------

evalService :: Monad m => Evaluator a m b -> Service -> m b
evalService f = eval f . serviceFunction

data Evaluator a m b where
   Evaluator :: (TypedValue (Type a) -> m b)  -- encoder
             -> (forall t . Type a t -> m t)  -- decoder
             -> Evaluator a m b

{-
type Fix a = a -> a

encodeTypeRep :: Monoid a => (TypedValue f -> a) -> TypedValue (TypeRep f) -> a
encodeTypeRep = fix . encodeTypeRepFix

encodeTypeRepFix :: Monoid a => (TypedValue f -> a) -> Fix (TypedValue (TypeRep f) -> a)
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

encodeWith :: (Monad m, Typed a t) => (t -> m b) -> TypedValue (Type a) -> m b
encodeWith enc (val ::: tp) =
   case equal tp typed of
      Just f  -> enc (f val)
      Nothing -> fail "encoding failed" -}

eval :: Monad m => Evaluator a m b -> TypedValue (Type a) -> m b
eval f@(Evaluator enc dec) tv@(val ::: tp) =
   case tp of
      -- handle exceptions
      Const String :|: t ->
         either fail (\a -> eval f (a ::: t)) val
      -- uncurry function if possible
      t1 :-> t2 :-> t3 ->
         eval f (uncurry val ::: Pair t1 t2 :-> t3)
      t1 :-> t2 -> do
         a <- dec t1
         eval f (val a ::: t2)
      _ ->
         enc tv