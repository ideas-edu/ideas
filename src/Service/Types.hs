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
module Service.Types
   ( -- * Services
     Service, makeService, deprecate
   , serviceDeprecated, serviceFunction
     -- * Types
   , Type(..), TypedValue(..), tuple2, tuple3, tuple4
   , maybeType, optionType
   , errorType, difficultyType, listType, envType, elemType
   , derivationType, messageType
   , equal, equalM
   ) where

import Common.Library
import Control.Monad
import Data.List
import Data.Maybe
import Service.FeedbackScript.Syntax
import System.Random

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
equal type1 type2 =
   case (type1, type2) of
      (Pair a b,    Pair c d   ) -> liftM2 (\f g (x, y) -> (f x, g y)) (equal a c) (equal b d)
      (a :|: b,     c :|: d    ) -> liftM2 biMap (equal a c) (equal b d)
      (List a,      List b     ) -> fmap map (equal a b)
      (Rule,        Rule       ) -> Just id
      (Unit,        Unit       ) -> Just id
      (StrategyCfg, StrategyCfg) -> Just id
      (Location,    Location   ) -> Just id
      (Id,          Id         ) -> Just id
      (Term,        Term       ) -> Just id
      (Exercise,    Exercise   ) -> Just id
      (Script,      Script     ) -> Just id
      (Context,     Context    ) -> Just id
      (BindingTp,   BindingTp  ) -> Just id
      (Text,        Text       ) -> Just id
      (IO a,        IO b       ) -> fmap liftM (equal a b)
      (Exception,   Exception  ) -> Just id
      (Bool,        Bool       ) -> Just id
      (String,      String     ) -> Just id
      (Int,         Int        ) -> Just id
      (Iso p a,     _          ) -> fmap (. to p) (equal a type2)
      (_,           Iso p b    ) -> fmap (from p .) (equal type1 b)
      (Tag s1 a,    Tag s2 b   ) -> guard (s1==s2) >> equal a b
      _                          -> Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue a = forall t . t ::: Type a t

tuple2 :: Type a t1 -> Type a t2 -> Type a (t1, t2)
tuple2 = Pair

tuple3 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a (t1, t2, t3)
tuple3 t1 t2 t3 = Iso (f <-> g) (Pair t1 (Pair t2 t3))
 where
   f (a, (b, c)) = (a, b, c)
   g (a, b, c)   = (a, (b, c))

tuple4 :: Type a t1 -> Type a t2 -> Type a t3 -> Type a t4 -> Type a (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 t4)))
 where
   f (a, (b, (c, d))) = (a, b, c, d)
   g (a, b, c, d)     = (a, (b, (c, d)))

maybeType :: Type a t1 -> Type a (Maybe t1)
maybeType t = Iso (f <-> g) (t :|: Unit)
 where
   f = either Just (const Nothing)
   g = maybe (Right ()) Left

optionType :: t1 -> Type a t1 -> Type a t1
optionType a t = Iso (fromMaybe a <-> Just) (maybeType t)

errorType :: Type a t -> Type a (Either String t)
errorType t = Exception :|: t

listType :: Type a t -> Type a [t] -- with list "tag"
listType = Tag "list" . List . elemType

envType :: Type a Environment
envType = Iso (makeEnvironment <-> bindings) (List BindingTp)

elemType :: Type a t -> Type a t
elemType = Tag "elem"

messageType :: Type a t -> Type a t
messageType = Tag "message"

difficultyType :: Type a Difficulty
difficultyType = Tag "difficulty" (Iso (f <-> show) String)
 where
   f = fromMaybe Medium . readDifficulty

derivationType :: Type a t1 -> Type a t2 -> Type a (Derivation t1 t2)
derivationType t1 t2 = Iso (f <-> g) (listType (tuple2 t1 t2))
 where
   f = foldl extend (emptyDerivation (error "derivationType") )
   g = map (\(_, s, a) -> (s, a)) . triples

data Type a t where
   -- Type isomorphisms (for defining type synonyms)
   Iso          :: Isomorphism t1 t2 -> Type a t1 -> Type a t2
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
   Exception    :: Type a String
   -- Exercise-specific types
   Exercise     :: Type a (Exercise a)
   Script       :: Type a Script
   Strategy     :: Type a (Strategy (Context a))
   Rule         :: Type a (Rule (Context a))
   Term         :: Type a a
   Context      :: Type a (Context a)
   Location     :: Type a Location
   Id           :: Type a Id
   StrategyCfg  :: Type a StrategyConfiguration
   BindingTp    :: Type a (Typed Binding)
   Text         :: Type a Text
   -- Basic types
   Bool         :: Type a Bool
   Int          :: Type a Int
   String       :: Type a String

instance Show (Type a t) where
   show (Iso _ t)      = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s -- ++ "@(" ++ show t ++ ")"
   show (List t)       = "[" ++ show t ++ "]"
   show (IO t)         = show t
   show t              = fromMaybe "unknown" (showGroundType t)

showTuple :: Type a t -> String
showTuple tp = "(" ++ intercalate ", " (collect tp) ++ ")"
 where
   collect :: Type a t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ t)    = collect t
   collect t            = [show t]

showGroundType :: Type a t -> Maybe String
showGroundType tp =
   case tp of
      Exercise     -> Just "Exercise"
      Script       -> Just "Script"
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
      BindingTp    -> Just "Binding"
      Text         -> Just "TextMessage"
      Exception    -> Just "Exception"
      _            -> Nothing