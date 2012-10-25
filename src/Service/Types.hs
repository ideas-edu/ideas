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
   , TypeRep(..), Const(..), Type, TypedValue(..), tuple2, tuple3, tuple4
   , stringType, intType, boolType
   , exerciseType, strategyType, ruleType, termType, contextType
   , scriptType, locationType, idType, bindingType, strategyCfgType
   , textType, stdGenType
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

class Equal f where
   equal :: f a -> f b -> Maybe (a -> b)

equalM :: Monad m => Type a t1 -> Type a t2 -> m (t1 -> t2)
equalM t1 t2 = maybe (fail msg) return (equal t1 t2)
 where msg = "Types not equal: " ++ show t1 ++ " and " ++ show t2

instance Equal f => Equal (TypeRep f) where
   equal (Iso p a)  t2         = fmap (. to p) (equal a t2)
   equal t1         (Iso p b)  = fmap (from p .) (equal t1 b)
   equal (Pair a b) (Pair c d) = do f <- equal a c
                                    g <- equal b d
                                    return $ \(x, y) -> (f x, g y)
   equal (a :|: b)  (c :|: d)  = liftM2 biMap (equal a c) (equal b d)
   equal (List a)   (List b)   = fmap map (equal a b)
   equal (Tag s1 a) (Tag s2 b) | s1 == s2 = equal a b
   equal Unit       Unit       = Just id
   equal (Const a)  (Const b)  = equal a b
   equal _          _          = Nothing

instance Equal (Const a) where
   equal Int       Int       = Just id
   equal Bool      Bool      = Just id
   equal String    String    = Just id
   equal Exercise  Exercise  = Just id
   equal Strategy  Strategy  = Just id
   equal Rule      Rule      = Just id
   equal Term      Term      = Just id
   equal Context   Context   = Just id
   equal Script    Script    = Just id
   equal Location  Location  = Just id     
   equal Id        Id        = Just id     
   equal StratCfg  StratCfg  = Just id     
   equal BindingTp BindingTp = Just id     
   equal Text      Text      = Just id     
   equal StdGen    StdGen    = Just id     
   equal _         _         = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue a = forall t . t ::: Type a t

tuple2 :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1, t2)
tuple2 = Pair

tuple3 :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f t3 -> TypeRep f (t1, t2, t3)
tuple3 t1 t2 t3 = Iso (f <-> g) (Pair t1 (Pair t2 t3))
 where
   f (a, (b, c)) = (a, b, c)
   g (a, b, c)   = (a, (b, c))

tuple4 :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f t3 -> TypeRep f t4 -> TypeRep f (t1, t2, t3, t4)
tuple4 t1 t2 t3 t4 = Iso (f <-> g) (Pair t1 (Pair t2 (Pair t3 t4)))
 where
   f (a, (b, (c, d))) = (a, b, c, d)
   g (a, b, c, d)     = (a, (b, (c, d)))

exerciseType :: Type a (Exercise a)
exerciseType = Const Exercise

strategyType :: Type a (Strategy (Context a))
strategyType = Const Strategy

ruleType :: Type a (Rule (Context a))
ruleType = Const Rule

termType :: Type a a
termType = Const Term

contextType :: Type a (Context a)
contextType = Const Context

scriptType :: Type a Script
scriptType = Const Script

locationType :: Type a Location
locationType = Const Location

idType :: Type a Id
idType = Const Id

strategyCfgType :: Type a StrategyConfiguration
strategyCfgType = Const StratCfg

bindingType :: Type a Binding
bindingType = Const BindingTp

textType :: Type a Text
textType = Const Text

stdGenType :: Type a StdGen
stdGenType = Const StdGen

intType :: Type a Int
intType = Const Int

boolType :: Type a Bool
boolType = Const Bool

stringType :: Type a String
stringType = Const String

maybeType :: Type a t1 -> Type a (Maybe t1)
maybeType t = Iso (f <-> g) (t :|: Unit)
 where
   f = either Just (const Nothing)
   g = maybe (Right ()) Left

optionType :: t1 -> Type a t1 -> Type a t1
optionType a t = Iso (fromMaybe a <-> Just) (maybeType t)

errorType :: Type a t -> Type a (Either String t)
errorType t = Tag "Exception" stringType :|: t

listType :: Type a t -> Type a [t] -- with list "tag"
listType = Tag "list" . List . elemType

envType :: Type a Environment
envType = Iso (makeEnvironment <-> bindings) (List bindingType)

elemType :: Type a t -> Type a t
elemType = Tag "elem"

messageType :: Type a t -> Type a t
messageType = Tag "message"

difficultyType :: Type a Difficulty
difficultyType = Tag "difficulty" (Iso (f <-> show) stringType)
 where
   f = fromMaybe Medium . readDifficulty

derivationType :: Type a t1 -> Type a t2 -> Type a (Derivation t1 t2)
derivationType t1 t2 = Iso (f <-> g) (listType (tuple2 t1 t2))
 where
   f = foldl extend (emptyDerivation (error "derivationType") )
   g = map (\(_, s, a) -> (s, a)) . triples

type Type a = TypeRep (Const a)

data TypeRep f t where
   -- Type isomorphisms (for defining type synonyms)
   Iso   :: Isomorphism t1 t2 -> TypeRep f t1 -> TypeRep f t2
   -- Function type
   (:->) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1 -> t2)
   -- Special annotations
   Tag   :: String -> TypeRep f t1 -> TypeRep f t1
   -- Type constructors
   List  :: TypeRep f t  -> TypeRep f [t]
   Pair  :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1, t2)
   (:|:) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (Either t1 t2)
   Unit  :: TypeRep f ()
   -- Type constants
   Const :: f t -> TypeRep f t

data Const a t where
   -- exercise specific
   Exercise  :: Const a (Exercise a)
   Strategy  :: Const a (Strategy (Context a))
   Rule      :: Const a (Rule (Context a))
   Term      :: Const a a
   Context   :: Const a (Context a)
   -- other types
   Script    :: Const a Script
   Location  :: Const a Location
   Id        :: Const a Id
   StratCfg  :: Const a StrategyConfiguration
   BindingTp :: Const a Binding
   Text      :: Const a Text
   StdGen    :: Const a StdGen
   -- basic types
   Bool      :: Const a Bool
   Int       :: Const a Int
   String    :: Const a String

class ShowF f where
   showF :: f a -> String

instance ShowF f => ShowF (TypeRep f) where
   showF = show

instance ShowF f => Show (TypeRep f t) where
   show (Iso _ t)      = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s -- ++ "@(" ++ show t ++ ")"
   show (List t)       = "[" ++ show t ++ "]"
   show Unit           = "()"
   show (Const c)      = showF c

instance Show (Const a t) where
   show = showF

instance ShowF (Const a) where
   showF Exercise  = "Exercise"
   showF Strategy  = "Strategy"
   showF Rule      = "Rule"
   showF Term      = "Term"
   showF Context   = "Context"
   showF Script    = "Script"
   showF Location  = "Location"
   showF Id        = "Id"
   showF StratCfg  = "StrategyConfiguration"
   showF BindingTp = "Binding"
   showF Text      = "TextMessage"
   showF StdGen    = "StdGen"
   showF Bool      = "Bool"
   showF Int       = "Int" 
   showF String    = "String"

showTuple :: ShowF f => TypeRep f t -> String
showTuple tp = "(" ++ intercalate ", " (collect tp) ++ ")"
 where
   collect :: ShowF f => TypeRep f t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ t)    = collect t
   collect t            = [showF t]