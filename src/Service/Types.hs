{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.Types where

import Common.Context (Context, Location, fromContext)
import Common.Exercise (Exercise)
import Common.Transformation (Rule, name)
import Common.Strategy (StrategyLocation, StrategyConfiguration)
import Common.Utils (commaList)
import Control.Arrow
import Control.Monad
import Data.Maybe
import Service.ExerciseList (ExercisePackage, exercise, getExerciseText)
import Service.TypedAbstractService (State, Result)
import Service.Diagnose (Diagnosis)
import Service.FeedbackText (ExerciseText)
import System.IO.Unsafe
import qualified Text.OpenMath.Reply as Decomposition

infix  2 :::
infixr 3 :->

data TypedValue a = forall t . t ::: Type a t

data Type a t where
   -- Function type
   (:->)        :: Type a t1 -> Type a t2 -> Type a (t1 -> t2)
   -- Tuple types
   Tuple        :: Type a t1 -> Type a t2 -> Type a (t1, t2)
   Triple       :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
   Quadruple    :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a (t1, t2, t3, t4)
   -- Special annotations
   Tag          :: String -> Type a t1 -> Type a t1
   Optional     :: t1 -> Type a t1 -> Type a t1
   Maybe        :: Type a t1 -> Type a (Maybe t1)
   -- Type constructors
   List         :: Type a t -> Type a [t]
   Elem         :: Type a t -> Type a t -- quick fix
   IO           :: Type a t -> Type a (IO t)
   -- Exercise-specific types
   State        :: Type a (State a)
   Exercise     :: Type a (Exercise a)
   ExerciseText :: Type a (ExerciseText a)
   Rule         :: Type a (Rule (Context a))
   Term         :: Type a a
   Context      :: Type a (Context a)
   Result       :: Type a (Result a)
   Diagnosis    :: Type a (Diagnosis a)
   Location     :: Type a Location
   StrategyLoc  :: Type a StrategyLocation
   StrategyCfg  :: Type a StrategyConfiguration
   DecompositionReply :: Type a (Decomposition.Reply a)
   -- Basic types
   Bool         :: Type a Bool
   Int          :: Type a Int
   String       :: Type a String
   

instance Show (Type a t) where
   show (t1 :-> t2)       = show t1 ++ " -> " ++ show t2 
   show (Tuple t1 t2)     = "(" ++ commaList [show t1, show t2] ++ ")"
   show (Triple t1 t2 t3) = "(" ++ commaList [show t1, show t2, show t3] ++ ")"
   show (Quadruple t1 t2 t3 t4) = "(" ++ commaList [show t1, show t2, show t3, show t4] ++ ")"
   show (Tag _ t)         = show t
   show (Optional _ t)    = "(" ++ show t ++ ")?"
   show (Maybe t)         = "(" ++ show t ++ ")?"
   show (List t)          = "[" ++ show t ++ "]"
   show (Elem t)          = show t
   show (IO t)            = show t
   show t                 = fromMaybe "unknown" (groundType t)

groundType :: Type a t -> Maybe String
groundType tp =
   case tp of 
      State        -> Just "State"
      Exercise     -> Just "Exercise"
      ExerciseText -> Just "ExerciseText"
      Rule         -> Just "Rule"
      Term         -> Just "Term"
      Context      -> Just "Context"
      Result       -> Just "Result"
      Diagnosis    -> Just "Diagnosis"
      Bool         -> Just "Bool"
      Int          -> Just "Int"
      String       -> Just "String"
      Location     -> Just "Location"
      StrategyLoc  -> Just "StrategyLocation"
      StrategyCfg  -> Just "StrategyConfiguration"
      _            -> Nothing

data Evaluator m inp out a = Evaluator 
   { encoder :: Encoder m out a
   , decoder :: Decoder m inp a
   }

data Encoder m s a = Encoder 
   { encodeType  :: forall t . Type a t -> t -> m s
   , encodeTerm  :: a -> m s
   , encodeTuple :: [s] -> s
   }

data Decoder m s a = Decoder 
   { decodeType     :: forall t . Type a t -> s -> m (t, s)
   , decodeTerm     :: s -> m a
   , decoderPackage :: ExercisePackage a
   }

decoderExercise :: Decoder m s a -> Exercise a
decoderExercise = exercise . decoderPackage

eval :: Monad m => Evaluator m inp out a -> TypedValue a -> inp -> m out
eval f (tv ::: tp) s = 
   case tp of 
      t1 :-> t2 -> do
         (a, s1) <- decodeType (decoder f) t1 s
         eval f (tv a ::: t2) s1
      _ ->
         encodeType (encoder f) tp tv

decodeDefault :: MonadPlus m => Decoder m s a -> Type a t -> s -> m (t, s)
decodeDefault dec tp s =
   case tp of
      Tuple t1 t2 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         return ((a, b), s2)
      Triple t1 t2 t3 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         (c, s3) <- decodeType dec t3 s2
         return ((a, b, c), s3)
      Quadruple t1 t2 t3 t4 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         (c, s3) <- decodeType dec t3 s2
         (d, s4) <- decodeType dec t4 s3
         return ((a, b, c, d), s4)
      Tag _ t1 ->
         decodeType dec t1 s
      Optional a t1 -> 
         decodeType dec t1 s `mplus` return (a, s)
      Maybe t1 -> 
         liftM (first Just) (decodeType dec t1 s) `mplus` return (Nothing, s)
      Exercise -> do
         return (exercise (decoderPackage dec), s)
      ExerciseText -> do
         exText <- case getExerciseText (decoderPackage dec) of 
                      Just a  -> return a
                      Nothing -> fail "No support for exercise texts"
         return (exText, s)
      _ ->
         fail $ "No support for argument type: " ++ show tp

encodeDefault :: Monad m => Encoder m s a -> Type a t -> t -> m s
encodeDefault enc tp tv =
   case tp of
      Tuple t1 t2 -> do
         let (a, b) = tv
         x <- encodeType enc t1 a
         y <- encodeType enc t2 b
         return (encodeTuple enc [x, y])
      Triple t1 t2 t3 -> do
         let (a, b, c) = tv
         x <- encodeType enc t1 a
         y <- encodeType enc t2 b
         z <- encodeType enc t3 c
         return (encodeTuple enc [x, y, z])
      Quadruple t1 t2 t3 t4 -> do
         let (a, b, c, d) = tv
         x <- encodeType enc t1 a
         y <- encodeType enc t2 b
         z <- encodeType enc t3 c
         u <- encodeType enc t4 d
         return (encodeTuple enc [x, y, z, u])
      Tag _ t1      -> encodeType enc t1 tv
      Elem t1       -> encodeType enc t1 tv
      Optional _ t1 -> encodeType enc t1 tv
      Maybe t1      -> case tv of
                          Just a  -> encodeType enc t1 a
                          Nothing -> return (encodeTuple enc [])
      IO t1         -> encodeType enc t1 (unsafePerformIO tv)
      Rule          -> encodeType enc String (name tv)
      Term          -> encodeTerm enc tv
      Context       -> encodeType enc Term (fromContext tv)
      Location      -> encodeType enc String (show tv)
      _             -> fail "No support for result type"