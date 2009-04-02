{-# OPTIONS -XGADTs -XRank2Types #-}
module Service.Types where

import Common.Context (Context, Location)
import Common.Exercise (Exercise)
import Common.Transformation (Rule)
import Common.Utils (commaList)
import Data.Maybe
import Service.TypedAbstractService (State, Result)
import System.IO.Unsafe

infixr 1 :->

data Type a t where
   -- Function type
   (:->)    :: Type a t1 -> Type a t2 -> Type a (t1 -> t2)
   -- Tuple types
   Pair     :: Type a t1 -> Type a t2 -> Type a (t1, t2)
   Triple   :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
   -- Type constructors
   List     :: Type a t -> Type a [t]
   Elem     :: Type a t -> Type a t -- quick fix
   IO       :: Type a t -> Type a (IO t)
   -- Exercise-specific types
   State    :: Type a (State a)
   Exercise :: Type a (Exercise a)
   Rule     :: Type a (Rule (Context a))
   Term     :: Type a (Context a)
   Result   :: Type a (Result a)
   -- Basic types
   Bool     :: Type a Bool
   Int      :: Type a Int
   String   :: Type a String
   Location :: Type a Location

instance Show (Type a t) where
   show (t1 :-> t2)       = show t1 ++ " -> " ++ show t2 
   show (Pair t1 t2)      = "(" ++ commaList [show t1, show t2] ++ ")"
   show (Triple t1 t2 t3) = "(" ++ commaList [show t1, show t2, show t3] ++ ")"
   show (List t)          = "[" ++ show t ++ "]"
   show (Elem t)          = show t
   show (IO t)            = show t
   show t                 = fromMaybe "??" (groundType t)

groundType :: Type a t -> Maybe String
groundType tp =
   case tp of 
      State    -> Just "State"
      Exercise -> Just "Exercise"
      Rule     -> Just "Rule"
      Term     -> Just "Term"
      Result   -> Just "Result"
      Bool     -> Just "Bool"
      Int      -> Just "Int"
      String   -> Just "String"
      Location -> Just "Location"
      _        -> Nothing

eqType :: Type a1 t1 -> Type a2 t2 -> Bool
eqType (t1 :-> t2)       (t3 :-> t4)       = eqType t1 t3 && eqType t2 t4
eqType (Pair t1 t2)      (Pair t3 t4)      = eqType t1 t3 && eqType t2 t4
eqType (Triple t1 t2 t3) (Triple t4 t5 t6) = eqType t1 t4 && eqType t2 t5 && eqType t3 t6
eqType (List t1)         (List t2)         = eqType t1 t2
eqType (Elem t1)         (Elem t2)         = eqType t1 t2
eqType (IO t1)           (IO t2)           = eqType t1 t2 
eqType t1 t2 = maybe False ((groundType t1 ==) . Just) (groundType t2)

data Evaluator m inp out a = Evaluator 
   { encoder :: Encoder out a
   , decoder :: Decoder m inp a
   }

data Encoder s a = Encoder 
   { encodeType  :: forall t . Type a t -> t -> s
   , encodeTerm  :: a -> s
   , encodeTuple :: [s] -> s
   }

data Decoder m s a = Decoder 
   { decodeType :: forall t . Type a t -> s -> m (t, s)
   , decodeTerm :: s -> m a
   , decoderExercise :: Exercise a
   }

eval :: Monad m => Evaluator m inp out a -> Type a t -> t -> inp -> m out
eval f tp tv s = 
   case tp of 
      t1 :-> t2 -> do
         (a, s1) <- decodeType (decoder f) t1 s
         eval f t2 (tv a) s1
      _ ->
         return (encodeType (encoder f) tp tv)

decodeDefault :: Monad m => Decoder m s a -> Type a t -> s -> m (t, s)
decodeDefault dec tp s =
   case tp of
      Pair t1 t2 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         return ((a, b), s2)
      Triple t1 t2 t3 -> do
         (a, s1) <- decodeType dec t1 s
         (b, s2) <- decodeType dec t2 s1
         (c, s3) <- decodeType dec t3 s2
         return ((a, b, c), s3)
      _ ->
         error "No support for argument type"

encodeDefault :: Encoder s a -> Type a t -> t -> s
encodeDefault enc tp tv = do
   case tp of
      Pair t1 t2 ->
         let (a, b) = tv
         in encodeTuple enc [encodeType enc t1 a, encodeType enc t2 b]
      Triple t1 t2 t3 ->
         let (a, b, c) = tv
         in encodeTuple enc [encodeType enc t1 a, encodeType enc t2 b, encodeType enc t3 c]
      IO t1 ->
         encodeType enc t1 (unsafePerformIO tv)
      _ -> 
         error "No support for result type"