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
module Domain.Logic.Formula
   ( module Domain.Logic.Formula
   , conjunctions, disjunctions, ors, ands
   ) where

import Common.Algebra.Boolean
import Common.Algebra.CoBoolean
import Common.Classes
import Common.Rewriting
import Common.Utils (ShowString, subsets)
import Common.Utils.Uniplate
import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable, foldMap, toList)
import Data.List
import qualified Data.Traversable as T
import qualified Text.OpenMath.Dictionary.Logic1 as OM

infixr 2 :<->:
infixr 3 :->:
infixr 4 :||:
infixr 5 :&&:

-- | The data type Logic is the abstract syntax for the domain
-- | of logic expressions.
data Logic a = Var a
             | Logic a :->:  Logic a            -- implication
             | Logic a :<->: Logic a            -- equivalence
             | Logic a :&&:  Logic a            -- and (conjunction)
             | Logic a :||:  Logic a            -- or (disjunction)
             | Not (Logic a)                    -- not
             | T                                -- true
             | F                                -- false
 deriving (Eq, Ord)

-- | For simple use, we assume the variables to be strings
type SLogic = Logic ShowString

instance Show a => Show (Logic a) where
   show = ppLogic

instance Functor Logic where
   fmap = T.fmapDefault

instance Foldable Logic where
   foldMap = T.foldMapDefault

instance T.Traversable Logic where
   traverse f = foldLogic
      ( fmap Var . f, liftA2 (:->:), liftA2 (:<->:), liftA2 (:&&:)
      , liftA2 (:||:), liftA Not, pure T, pure F
      )

instance BoolValue (Logic a) where
   fromBool b = if b then T else F
   isTrue T  = True
   isTrue _  = False
   isFalse F = True
   isFalse _ = False

instance Boolean (Logic a) where
   (<&&>)     = (:&&:)
   (<||>)     = (:||:)
   complement = Not

instance CoBoolean (Logic a) where
   isAnd (p :&&: q)     = Just (p, q)
   isAnd _              = Nothing
   isOr  (p :||: q)     = Just (p, q)
   isOr  _              = Nothing
   isComplement (Not p) = Just p
   isComplement _       = Nothing

instance Container Logic where
   singleton            = Var
   getSingleton (Var a) = Just a
   getSingleton _       = Nothing

-- | The type LogicAlg is the algebra for the data type Logic
-- | Used in the fold for Logic.
type LogicAlg b a = (b -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a -> a, a -> a, a, a)

-- | foldLogic is the standard fold for Logic.
foldLogic :: LogicAlg b a -> Logic b -> a
foldLogic (var, impl, equiv, conj, disj, neg, tr, fl) = rec
 where
   rec logic =
      case logic of
         Var x     -> var x
         p :->: q  -> rec p `impl`  rec q
         p :<->: q -> rec p `equiv` rec q
         p :&&: q  -> rec p `conj`  rec q
         p :||: q  -> rec p `disj`  rec q
         Not p     -> neg (rec p)
         T         -> tr
         F         -> fl

-- | Pretty-printer for propositions
ppLogic :: Show a => Logic a -> String
ppLogic = ppLogicPrio 0

ppLogicPrio :: Show a => Int -> Logic a -> String
ppLogicPrio = (\f s -> f s "") . flip (foldLogic alg)
 where
   alg = ( pp . show, binop 3 "->", binop 0 "<->", binop 2 "/\\"
         , binop 1 "||", nott, pp "T", pp "F")
   binop prio op p q n = parIf (n > prio) (p (prio+1) . ((" "++op++" ")++) . q prio)
   pp s      = const (s++)
   nott p _  = ("~"++) . p 4
   parIf b f = if b then ("("++) . f . (")"++) else f

-- | The monadic join for logic
catLogic :: Logic (Logic a) -> Logic a
catLogic = foldLogic (id, (:->:), (:<->:), (:&&:), (:||:), Not, T, F)

-- | evalLogic takes a function that gives a logic value to a variable,
-- | and a Logic expression, and evaluates the boolean expression.
evalLogic :: (a -> Bool) -> Logic a -> Bool
evalLogic env = foldLogic (env, impl, (==), (&&), (||), not, True, False)
 where
   impl p q = not p || q

-- | eqLogic determines whether or not two Logic expression are logically
-- | equal, by evaluating the logic expressions on all valuations.
eqLogic :: Eq a => Logic a -> Logic a -> Bool
eqLogic p q = all (\f -> evalLogic f p == evalLogic f q) fs
 where
   xs = varsLogic p `union` varsLogic q
   fs = map (flip elem) (subsets xs)

-- | A Logic expression is atomic if it is a variable or a constant True or False.
isAtomic :: Logic a -> Bool
isAtomic logic =
   case logic of
      Not (Var _) -> True
      _           -> null (children logic)

-- | Functions isDNF, and isCNF determine whether or not a Logix expression
-- | is in disjunctive normal form, or conjunctive normal form, respectively.
isDNF, isCNF :: Logic a -> Bool
isDNF = all isAtomic . concatMap conjunctions . disjunctions
isCNF = all isAtomic . concatMap disjunctions . conjunctions

-- | Count the number of equivalences
countEquivalences :: Logic a -> Int
countEquivalences p = length [ () | _ :<->: _ <- universe p ]

-- | Function varsLogic returns the variables that appear in a Logic expression.
varsLogic :: Eq a => Logic a -> [a]
varsLogic = nub . toList

instance Uniplate (Logic a) where
   uniplate this =
      case this of
         p :->: q  -> plate (:->:)  |* p |* q
         p :<->: q -> plate (:<->:) |* p |* q
         p :&&: q  -> plate (:&&:)  |* p |* q
         p :||: q  -> plate (:||:)  |* p |* q
         Not p     -> plate Not     |* p
         _         -> plate this

instance Different (Logic a) where
   different = (T, F)

instance IsTerm a => IsTerm (Logic a) where
   toTerm = foldLogic
      ( toTerm, binary impliesSymbol, binary equivalentSymbol
      , binary andSymbol, binary orSymbol, unary notSymbol
      , symbol trueSymbol, symbol falseSymbol
      )

   fromTerm a =
      fromTermWith f a `mplus` liftM Var (fromTerm a)
    where
      f s []
         | s == trueSymbol       = return T
         | s == falseSymbol      = return F
      f s [x]
         | s == notSymbol        = return (Not x)
      f s [x, y]
         | s == impliesSymbol    = return (x :->: y)
         | s == equivalentSymbol = return (x :<->: y)
      f s xs
         | s == andSymbol        = return (ands xs)
         | s == orSymbol         = return (ors xs)
      f _ _ = fail "fromTerm"

trueSymbol, falseSymbol, notSymbol, impliesSymbol, equivalentSymbol,
   andSymbol, orSymbol :: Symbol

trueSymbol       = newSymbol OM.trueSymbol
falseSymbol      = newSymbol OM.falseSymbol
notSymbol        = newSymbol OM.notSymbol
impliesSymbol    = newSymbol OM.impliesSymbol
equivalentSymbol = newSymbol OM.equivalentSymbol
andSymbol        = makeAssociative $ newSymbol OM.andSymbol
orSymbol         = makeAssociative $ newSymbol OM.orSymbol