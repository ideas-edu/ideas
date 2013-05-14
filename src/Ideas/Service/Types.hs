{-# LANGUAGE GADTs, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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
module Ideas.Service.Types
   ( -- * Services
     Service, makeService, deprecate
   , serviceDeprecated, serviceFunction
     -- * Types
   , TypeRep(..), Const(..), Type, TypedValue(..)
   , tuple2, tuple3, tuple4
   , stringType, intType, boolType
   , exerciseType, strategyType, ruleType, contextType
   , scriptType, locationType, strategyCfgType
   , textType, stdGenType
   , maybeType, optionType
   , errorType, difficultyType, listType, envType, elemType, treeType
   , derivationType, stateType, StepInfo, stepInfoType
   , Equal(..), ShowF(..), equalM
   , Typed(..)
   ) where

import Ideas.Common.Library
import Ideas.Common.Utils
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import System.Random

-----------------------------------------------------------------------------
-- Services

data Service = Service
   { serviceId         :: Id
   , serviceDeprecated :: Bool
   , serviceFunction   :: forall a . TypedValue (Type a)
   }

instance HasId Service where
   getId = serviceId
   changeId f a = a { serviceId = f (serviceId a) }

makeService :: String -> String -> (forall a . TypedValue (Type a)) -> Service
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
   equal (Tree a)   (Tree b)   = fmap fmap (equal a b)
   equal (Tag s1 a) (Tag s2 b) | s1 == s2 = equal a b
   equal Unit       Unit       = Just id
   equal (Const a)  (Const b)  = equal a b
   equal _          _          = Nothing

instance Equal (Const a) where
   equal Int         Int         = Just id
   equal Bool        Bool        = Just id
   equal String      String      = Just id
   equal Exercise    Exercise    = Just id
   equal Strategy    Strategy    = Just id
   equal State       State       = Just id
   equal Rule         Rule       = Just id
   equal Context     Context     = Just id
   equal (Derivation a b) (Derivation c d) = liftM2 biMap (equal a c) (equal b d)
   equal Id          Id          = Just id
   equal Location    Location    = Just id
   equal Script      Script      = Just id
   equal StratCfg    StratCfg    = Just id     
   equal Environment Environment = Just id   
   equal SomeExercise SomeExercise = Just id
   equal Text        Text        = Just id     
   equal StdGen      StdGen      = Just id     
   equal _           _           = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue f = forall t . t ::: f t

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

contextType :: Type a (Context a)
contextType = Const Context

scriptType :: Type a Script
scriptType = Const Script

locationType :: Type a Location
locationType = Const Location

strategyCfgType :: Type a StrategyConfiguration
strategyCfgType = Const StratCfg

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
errorType t = stringType :|: t

listType :: Type a t -> Type a [t] -- with list "tag"
listType = Tag "list" . List . elemType

treeType :: Type a t -> Type a (Tree t) -- with tree "tag"                                                                            
treeType = Tag "tree" . Tree . elemType

envType :: Type a Environment
envType = Const Environment

elemType :: Type a t -> Type a t
elemType = Tag "elem"

difficultyType :: Type a Difficulty
difficultyType = Tag "difficulty" (Iso (f <-> show) stringType)
 where
   f = fromMaybe Medium . readDifficulty

derivationType :: Type a t1 -> Type a t2 -> Type a (Derivation t1 t2)
derivationType t1 t2 = Const (Derivation t1 t2)

stateType :: Type a (State a)
stateType = Const State

type StepInfo a = (Rule (Context a), Location, Environment) -- find a good place

stepInfoType :: Type a (StepInfo a)
stepInfoType = tuple3 ruleType locationType envType -- what, where, how

--------------------------------------------------------------

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
   Tree  :: TypeRep f t  -> TypeRep f (Tree t)
   Pair  :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1, t2)
   (:|:) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (Either t1 t2)
   Unit  :: TypeRep f ()
   -- Type constants
   Const :: f t -> TypeRep f t

data Const a t where
   -- exercise specific
   Exercise     :: Const a (Exercise a)
   Strategy     :: Const a (Strategy (Context a))
   State        :: Const a (State a)
   Rule         :: Const a (Rule (Context a))
   Context      :: Const a (Context a)
   Derivation   :: TypeRep (Const a) t1 -> TypeRep (Const a) t2 -> Const a (Derivation t1 t2)
   -- other types
   Id           :: Const a Id
   Location     :: Const a Location
   Script       :: Const a Script
   StratCfg     :: Const a StrategyConfiguration
   Environment  :: Const a Environment
   Text         :: Const a Text
   StdGen       :: Const a StdGen
   SomeExercise :: Const a (Some Exercise)
   -- basic types
   Bool         :: Const a Bool
   Int          :: Const a Int
   String       :: Const a String

class ShowF f where
   showF :: f a -> String

instance ShowF f => ShowF (TypeRep f) where
   showF = show

instance ShowF f => Show (TypeRep f t) where
   show (Iso _ t)      = show t
   show (t1 :-> t2)    = show t1 ++ " -> " ++ show t2
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s
   show (List t)       = "[" ++ show t ++ "]"
   show (Tree t)       = "Tree " ++ show t
   show Unit           = "()"
   show (Const c)      = showF c

instance Show (TypedValue f) => Show (TypedValue (TypeRep f)) where
   show (val ::: tp) = 
      case tp of
         Iso iso t  -> show (to iso val ::: t)
         _ :-> _    -> "<<function>>"
         Tag _ t    -> show (val ::: t)
         List t     -> showAsList (map (show . (::: t)) val)
         Tree t     -> showAsTree (fmap (show . (::: t)) val)
         Pair t1 t2 -> "(" ++ show (fst val ::: t1) ++
                       "," ++ show (snd val ::: t2) ++ ")"
         t1 :|: t2  -> either (show . (::: t1)) (show . (::: t2)) val
         Unit       -> "()"
         Const t    -> show (val ::: t)

showAsList :: [String] -> String
showAsList xs = "[" ++ intercalate "," xs ++ "]"

showAsTree :: Tree String -> String
showAsTree (Node a ts)
   | null ts   = a
   | otherwise = a ++ "{" ++ intercalate "," (fmap showAsTree ts) ++ "}"

instance Show (TypedValue (Const a)) where
   show (val ::: tp) =
      case tp of
         Exercise         -> showId val
         Strategy         -> show val
         Rule             -> showId val
         Id               -> showId val
         SomeExercise     -> case val of Some ex -> showId ex
         State            -> show val
         Context          -> show (location val, environment val)
         Derivation t1 t2 -> show (biMap (show . (::: t1)) (show . (::: t2)) val)
         Location         -> show val
         Script           -> show val
         StratCfg         -> show val
         Environment      -> show val
         Text             -> show val
         StdGen           -> show val
         Bool             -> map toLower (show val)
         Int              -> show val
         String           -> val

instance Show (Const a t) where
   show = showF

instance ShowF (Const a) where
   showF Exercise     = "Exercise"
   showF Strategy     = "Strategy"
   showF State        = "State"
   showF Rule         = "Rule"
   showF Context      = "Context"
   showF (Derivation t1 t2) = "Derivation " ++ show t1 ++ " " ++ show t2
   showF Id           = "Id"
   showF Location     = "Location"
   showF Script       = "Script"
   showF StratCfg     = "StrategyConfiguration"
   showF Environment  = "Environment"
   showF Text         = "TextMessage"
   showF StdGen       = "StdGen"
   showF SomeExercise = "Exercise"
   showF Bool         = "Bool"
   showF Int          = "Int" 
   showF String       = "String"

showTuple :: ShowF f => TypeRep f t -> String
showTuple tp = "(" ++ intercalate ", " (collect tp) ++ ")"
 where
   collect :: ShowF f => TypeRep f t -> [String]
   collect (Pair t1 t2) = collect t1 ++ collect t2
   collect (Iso _ t)    = collect t
   collect t            = [showF t]
   
---------------------------------------------------------------

class Typed a t | t -> a where
   typeOf    :: t -> Type a t
   typed     :: Type a t
   typedList :: Type a [t]
   -- default implementation
   typeOf    = const typed
   typedList = List typed

instance Typed a Int where
   typed = Const Int
   
instance Typed a Bool where
   typed = Const Bool

instance Typed a () where
   typed = Unit

instance Typed a Char where
   typed     = Iso (head <-> return) stringType
   typedList = stringType

instance Typed a (Rule (Context a)) where
   typed = Const Rule

instance Typed a (Strategy (Context a)) where
   typed = Const Strategy

instance Typed a Id where
   typed = Const Id

instance Typed a Location where
   typed = Const Location

instance Typed a Environment where
   typed = envType

instance Typed a StdGen where
   typed = stdGenType
   
instance Typed a Difficulty where
   typed = difficultyType

instance Typed a (State a) where
   typed = Const State

instance Typed a (Exercise a) where
   typed = Const Exercise
   
instance Typed a (Context a) where
   typed = Const Context
   
instance Typed a StrategyConfiguration where
   typed = Const StratCfg
   
instance Typed a Script where
   typed = scriptType

instance Typed a Text where
   typed = textType
   
instance (Typed a t1, Typed a t2) => Typed a (t1, t2) where
   typed = Pair typed typed
   
instance (Typed a t1, Typed a t2, Typed a t3) => Typed a (t1, t2, t3) where
   typed = tuple3 typed typed typed
   
instance (Typed a t1, Typed a t2) => Typed a (t1 -> t2) where
   typed = typed :-> typed

instance Typed a t => Typed a (Maybe t) where
   typed = maybeType typed

instance (Typed a t1, Typed a t2) => Typed a (Either t1 t2) where
   typed = typed :|: typed

instance (Typed a t1, Typed a t2) => Typed a (Derivation t1 t2) where
   typed = Const (Derivation typed typed)
      
instance Typed a t => Typed a [t] where
   typed = typedList
   
instance Typed a (Some Exercise) where
   typed = Const SomeExercise