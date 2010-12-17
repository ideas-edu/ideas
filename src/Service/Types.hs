{-# LANGUAGE GADTs, Rank2Types #-}
-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.Types 
   ( -- * Services
     Service, makeService, deprecate 
   , serviceDeprecated, serviceFunction
     -- * Types
   , Type(..), TypedValue(..), tuple2, tuple3, tuple4, maybeTp, optionTp
   , errorTp, equal, isSynonym, useSynonym, TypeSynonym, typeSynonym
   , equalM
   ) where

import Common.Library
import Common.Utils (commaList)
import Control.Monad
import Data.Maybe
import Service.ExercisePackage

-----------------------------------------------------------------------------
-- Services

data Service = Service 
   { serviceId         :: Id
   , serviceDeprecated :: Bool
   , serviceFunction   :: forall a . TypedValue a
   }
   
instance HasId Service where
   getId = serviceId
   changeId f a = a { serviceId = f (serviceId a) }
   
makeService :: String -> String -> (forall a . TypedValue a) -> Service
makeService s descr f = describe descr (Service (newId s) False f)

deprecate :: Service -> Service
deprecate s = s { serviceDeprecated = True }

equalM :: Monad m => Type a t1 -> Type a t2 -> m (t1 -> t2)
equalM t1 t2 = maybe (fail msg) return (equal t1 t2)
 where msg = "Types not equal: " ++ show t1 ++ " and " ++ show t2

equal :: Type a t1 -> Type a t2 -> Maybe (t1 -> t2)
equal (a :|: b) (c :|: d) = liftM2 (\f g -> either (Left . f) (Right . g)) (equal a c) (equal b d)
equal (List a) (List b) = liftM map (equal a b)
equal Rule Rule = Just id
equal Unit Unit = Just id
equal (Pair a b) (Pair c d) = liftM2 (\f g (x, y) -> (f x, g y)) (equal a c) (equal b d)
equal StrategyCfg StrategyCfg = Just id
equal Location Location = Just id
equal Id Id = Just id
equal Term Term = Just id
equal ExercisePkg ExercisePkg = Just id
equal Context Context = Just id
equal Bool Bool = Just id
equal String String = Just id
equal Int Int = Just id
equal (Iso _ f a) b = fmap (. f) (equal a b)
equal a (Iso f _ b) = fmap (f .) (equal a b)
equal (Tag s1 a) (Tag s2 b) = guard (s1==s2) >> equal a b
equal _ _ = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue a = forall t . t ::: Type a t

tuple2 :: Type a t1 -> Type a t2 -> Type a (t1, t2)
tuple2 = Pair

tuple3 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
tuple3 t1 t2 t3 = Iso f g (Pair t1 (Pair t2 t3)) 
 where
   f (a, (b, c)) = (a, b, c)
   g (a, b, c)   = (a, (b, c))
   
tuple4 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Iso f g (Pair t1 (Pair t2 (Pair t3 t4))) 
 where
   f (a, (b, (c, d))) = (a, b, c, d)
   g (a, b, c, d)     = (a, (b, (c, d)))

maybeTp :: Type a t1 -> Type a (Maybe t1)
maybeTp t = Iso f g (t :|: Unit)
 where
   f = either Just (const Nothing)
   g = maybe (Right ()) Left

optionTp :: t1 -> Type a t1 -> Type a t1
optionTp a t = Iso (fromMaybe a) Just (maybeTp t)

errorTp :: Type a t -> Type a (Either String t)
errorTp t = Iso f g (t :|: IO Unit)
 where
   f = either Right (const (Left "errorTp"))
   g = either (Right . fail) Left

data Type a t where
   -- Type isomorphisms (for defining type synonyms)
   Iso          :: (t1 -> t2) -> (t2 -> t1) -> Type a t1 -> Type a t2
   -- Function type
   (:->)        :: Type a t1 -> Type a t2 -> Type a (t1 -> t2)
   -- Special annotations
   Tag          :: String -> Type a t1 -> Type a t1
   -- Type constructors
   List         :: Type a t -> Type a [t]
   Pair         :: Type a t1 -> Type a t2 -> Type a (t1, t2)
   (:|:)        :: Type a t1 -> Type a t2 -> Type a (Either t1 t2)
   Unit         :: Type a ()
   IO           :: Type a t -> Type a (IO t)
   -- Exercise-specific types
   ExercisePkg  :: Type a (ExercisePackage a)
   Strategy     :: Type a (Strategy (Context a))
   Rule         :: Type a (Rule (Context a))
   Term         :: Type a a
   Context      :: Type a (Context a)
   Location     :: Type a Location
   Id           :: Type a Id
   StrategyCfg  :: Type a StrategyConfiguration
   -- Basic types
   Bool         :: Type a Bool
   Int          :: Type a Int
   String       :: Type a String

instance Show (Type a t) where
   show (Iso _ _ t)    = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2 
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s -- ++ "@(" ++ show t ++ ")"
   show (List t)       = "[" ++ show t ++ "]"
   show (IO t)         = show t
   show t              = fromMaybe "unknown" (groundType t)
   
showTuple :: Type a t -> String
showTuple tp = "(" ++ commaList (collect tp) ++ ")"
 where
   collect :: Type a t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ _ t)  = collect t
   collect t            = [show t]
   
groundType :: Type a t -> Maybe String
groundType tp =
   case tp of 
      ExercisePkg  -> Just "ExercisePkg"
      Strategy     -> Just "Strategy"
      Rule         -> Just "Rule"
      Term         -> Just "Term"
      Context      -> Just "Context"
      Unit         -> Just "()"
      Bool         -> Just "Bool"
      Int          -> Just "Int"
      String       -> Just "String"
      Location     -> Just "Location"
      Id           -> Just "Id"
      StrategyCfg  -> Just "StrategyConfiguration"
      _            -> Nothing
      
-----------------------------------------------------------------------------
-- Type Synonyms

data TypeSynonym a t = TS 
   { synonymName :: String 
   , useSynonym  :: Type a t
   , isSynonym   :: Monad m => TypedValue a -> m t
   }
   
typeSynonym :: String -> (t2 -> t) -> (t -> t2) -> Type a t2 -> TypeSynonym a t
typeSynonym name to from tp = TS
   { synonymName = name
   , useSynonym  = Tag name (Iso to from tp)
   , isSynonym   = maybe (fail name) return . matchSynonym
   }
 where
   matchSynonym (a ::: t0) = do
      (s, t) <- isTag t0
      guard (s == name)
      f <- equal t tp
      return (to (f a))

isTag :: Type a t -> Maybe (String, Type a t)
isTag (Tag s t) = Just (s, t)
isTag _         = Nothing