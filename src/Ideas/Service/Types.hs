{-# LANGUAGE GADTs, Rank2Types, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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

module Ideas.Service.Types
   ( -- * Services
     Service, makeService, deprecate
   , serviceDeprecated, serviceFunction
     -- * Types
   , TypeRep(..), Const(..), Type, TypedValue(..), Typed(..)
   , Equal(..), ShowF(..), equalM
   ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Tree
import Ideas.Common.Library
import Ideas.Common.Utils
import Ideas.Service.FeedbackScript.Syntax
import Ideas.Service.State
import System.Random
import qualified Ideas.Common.Utils.TestSuite as TestSuite

-----------------------------------------------------------------------------
-- Services

data Service = S
   { serviceId         :: Id
   , serviceDeprecated :: Bool
   , serviceFunction   :: forall a . TypedValue (Type a)
   }

instance Show Service where
   show = showId

instance HasId Service where
   getId = serviceId
   changeId f a = a { serviceId = f (serviceId a) }

makeService :: String -> String -> (forall a . TypedValue (Type a)) -> Service
makeService s descr f = describe descr (S (newId s) False f)

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
   equal (a :-> b)  (c :-> d)  = liftM2 (\f g h -> g . h . f) 
                                        (equal c a) (equal b d)
   equal (Pair a b) (Pair c d) = liftM2 (***) (equal a c) (equal b d)
   equal (a :|: b)  (c :|: d)  = liftM2 biMap (equal a c) (equal b d)
   equal (List a)   (List b)   = fmap map (equal a b)
   equal (Tag s1 a) (Tag s2 b) | s1 == s2 = equal a b
   equal Unit       Unit       = Just id
   equal (Const a)  (Const b)  = equal a b
   equal _          _          = Nothing

instance Equal (Const a) where
   equal Int         Int         = Just id
   equal Bool        Bool        = Just id
   equal String      String      = Just id
   equal Service     Service     = Just id
   equal Exercise    Exercise    = Just id
   equal Strategy    Strategy    = Just id
   equal State       State       = Just id
   equal Rule         Rule       = Just id
   equal Context     Context     = Just id
   equal Id          Id          = Just id
   equal Location    Location    = Just id
   equal Script      Script      = Just id
   equal StratCfg    StratCfg    = Just id
   equal Environment Environment = Just id
   equal SomeExercise SomeExercise = Just id
   equal Text        Text        = Just id
   equal StdGen      StdGen      = Just id
   equal Result      Result      = Just id
   equal _           _           = Nothing

infixr 5 :|:

-----------------------------------------------------------------------------
-- Types

infix  2 :::
infixr 3 :->

data TypedValue f where
   (:::) :: t -> f t -> TypedValue f

type Type a = TypeRep (Const a)

data TypeRep f t where
   -- Type isomorphisms (for defining type synonyms)
   Iso   :: Isomorphism t1 t2 -> TypeRep f t1 -> TypeRep f t2
   -- Function type
   (:->) :: TypeRep f t1 -> TypeRep f t2 -> TypeRep f (t1 -> t2)
   -- Input/output
   IO    :: TypeRep f t -> TypeRep f (IO t)
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
   Service      :: Const a Service
   Exercise     :: Const a (Exercise a)
   Strategy     :: Const a (Strategy (Context a))
   State        :: Const a (State a)
   Rule         :: Const a (Rule (Context a))
   Context      :: Const a (Context a)
   -- other types
   Id           :: Const a Id
   Location     :: Const a Location
   Script       :: Const a Script
   StratCfg     :: Const a StrategyCfg
   Environment  :: Const a Environment
   Text         :: Const a Text
   StdGen       :: Const a StdGen
   Result       :: Const a TestSuite.Result
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
   show (IO t)         = show t
   show t@(Pair _ _)   = showTuple t
   show (t1 :|: t2)    = show t1 ++ " | " ++ show t2
   show (Tag s _)      = s
   show (List t)       = "[" ++ show t ++ "]"
   show Unit           = "()"
   show (Const c)      = showF c

instance Show (TypedValue f) => Show (TypedValue (TypeRep f)) where
   show (val ::: tp) =
      case tp of
         Iso iso t  -> show (to iso val ::: t)
         _ :-> _    -> "<<function>>"
         IO _       -> "<<io>>"
         Tag _ t    -> show (val ::: t)
         List t     -> showAsList (map (show . (::: t)) val)
         Pair t1 t2 -> "(" ++ show (fst val ::: t1) ++
                       "," ++ show (snd val ::: t2) ++ ")"
         t1 :|: t2  -> either (show . (::: t1)) (show . (::: t2)) val
         Unit       -> "()"
         Const t    -> show (val ::: t)

showAsList :: [String] -> String
showAsList xs = "[" ++ intercalate "," xs ++ "]"

instance Show (TypedValue (Const a)) where
   show (val ::: tp) =
      case tp of
         Service          -> showId val
         Exercise         -> showId val
         Strategy         -> show val
         Rule             -> showId val
         Id               -> showId val
         SomeExercise     -> case val of Some ex -> showId ex
         State            -> show val
         Context          -> show (location val, environment val)
         Location         -> show val
         Script           -> show val
         StratCfg         -> show val
         Environment      -> show val
         Text             -> show val
         StdGen           -> show val
         Result           -> show val
         Bool             -> map toLower (show val)
         Int              -> show val
         String           -> val

instance Show (Const a t) where
   show = showF

instance ShowF (Const a) where
   showF Service      = "Service"
   showF Exercise     = "Exercise"
   showF Strategy     = "Strategy"
   showF State        = "State"
   showF Rule         = "Rule"
   showF Context      = "Context"
   showF Id           = "Id"
   showF Location     = "Location"
   showF Script       = "Script"
   showF StratCfg     = "StrategyConfiguration"
   showF Environment  = "Environment"
   showF Text         = "TextMessage"
   showF StdGen       = "StdGen"
   showF Result       = "TestSuiteResult"
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
   typed     = Iso (head <-> return) typed
   typedList = Const String

instance Typed a (Rule (Context a)) where
   typed = Const Rule

instance Typed a (Strategy (Context a)) where
   typed = Const Strategy

instance Typed a Id where
   typed = Const Id

instance Typed a Location where
   typed = Const Location

instance Typed a Environment where
   typed = Const Environment

instance Typed a StdGen where
   typed = Const StdGen

instance Typed a TestSuite.Result where
   typed = Const Result

instance Typed a Difficulty where
   typed = Tag "Difficulty" (Iso (f <-> show) typed)
    where
      f = fromMaybe Medium . readDifficulty

instance Typed a Service where
   typed = Const Service

instance Typed a (State a) where
   typed = Const State

instance Typed a (Exercise a) where
   typed = Const Exercise

instance Typed a (Context a) where
   typed = Const Context

instance Typed a StrategyCfg where
   typed = Const StratCfg

instance Typed a Script where
   typed = Const Script

instance Typed a Text where
   typed = Const Text

instance (Typed a t1, Typed a t2) => Typed a (t1, t2) where
   typed = Pair typed typed

instance (Typed a t1, Typed a t2, Typed a t3) => Typed a (t1, t2, t3) where
   typed = Iso (f <-> g) (Pair typed (Pair typed typed))
    where
      f (a, (b, c)) = (a, b, c)
      g (a, b, c)   = (a, (b, c))

instance (Typed a t1, Typed a t2, Typed a t3, Typed a t4) => Typed a (t1, t2, t3, t4) where
   typed = Iso (f <-> g) (Pair typed (Pair typed (Pair typed typed)))
    where
      f (a, (b, (c, d))) = (a, b, c, d)
      g (a, b, c, d)     = (a, (b, (c, d)))

instance (Typed a t1, Typed a t2) => Typed a (t1 -> t2) where
   typed = typed :-> typed

instance Typed a t => Typed a (IO t) where
   typed = IO typed

instance Typed a t => Typed a (Maybe t) where
   typed = Iso (f <-> g) (typed :|: Unit)
    where
      f = either Just (const Nothing)
      g = maybe (Right ()) Left

instance (Typed a t1, Typed a t2) => Typed a (Either t1 t2) where
   typed = typed :|: typed

instance (Typed a t1, Typed a t2) => Typed a (Derivation t1 t2) where
   typed = Tag "Derivation" $ Iso (f <-> g) typed
    where
      f (a, xs) = foldl extend (emptyDerivation a) xs
      g d = (firstTerm d, [ (s, a) | (_, s, a) <- triples d ])

instance Typed a t => Typed a [t] where
   typed = typedList

instance Typed a t => Typed a (Tree t) where
   typed = Tag "Tree" $ Iso (f <-> g) typed
    where
      f = uncurry Node
      g (Node a xs) = (a, xs)

instance Typed a (Some Exercise) where
   typed = Const SomeExercise