{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2015, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Representation of a strategy as a cyclic tree with explicit fixed-points.
-- The nodes in the tree are named strategy combinators. The leafs are rules.
--
-----------------------------------------------------------------------------

module Ideas.Common.Strategy.StrategyTree
   ( -- * StrategyTree type synonym
     StrategyTree
     -- * Declarations (named combinators)
   , Decl, Combinator, associative, isAssociative, combinator
   ,  (.=.), applyDecl
     -- * Arities
   , Arity(..), Nullary(..), Unary(..), Binary(..), Nary(..)
   ) where

import Data.Function
import Data.Maybe
import Ideas.Common.CyclicTree
import Ideas.Common.Id
import Ideas.Common.Rule
import Ideas.Common.Strategy.Choice
import Ideas.Common.Strategy.Process
import Ideas.Common.View

infix 1 .=.

------------------------------------------------------------------------------

type StrategyTree a = CyclicTree (Decl Nary) (Rule a)

applyDecl :: Arity f => Decl f -> f (StrategyTree a)
applyDecl d = toArity (node (d {combinator = op}) . make)
 where
   op = Nary $ fromMaybe empty . listify (combinator d)

   make | isAssociative d = concatMap collect
        | otherwise       = id

   collect a  =
      case isNode a of
         Just (da, as) | getId da == getId d -> as
         _ -> [a]

------------------------------------------------------------------------------

type Combinator f = forall a . f (Process (Rule a))

data Decl f = C
   { declId        :: Id
   , combinator    :: Combinator f
   , isAssociative :: Bool
   }

instance Show (Decl f) where
   show = showId

instance Eq (Decl f) where
   (==) = (==) `on` getId

instance HasId (Decl f) where
   getId = declId
   changeId f d = d { declId = f (declId d) }

(.=.) :: IsId n => n -> Combinator f -> Decl f
n .=. f = C (newId n) f False

associative :: Decl f -> Decl f
associative c = c {isAssociative = True}

------------------------------------------------------------------------------

class Arity f where
   listify :: f a -> [a] -> Maybe a
   toArity :: ([a] -> a) -> f a
   liftIso :: Isomorphism a b -> f a -> f b

data Nullary a = Nullary { fromNullary :: a }
data Unary   a = Unary   { fromUnary   :: a -> a }
data Binary  a = Binary  { fromBinary  :: a -> a -> a }
data Nary    a = Nary    { fromNary    :: [a] -> a }

instance Arity Nullary where
   listify (Nullary a) [] = Just a
   listify _ _ = Nothing
   toArity f = Nullary (f [])
   liftIso p (Nullary a) = Nullary (from p a)

instance Arity Unary where
   listify (Unary f) [x] = Just (f x)
   listify _ _ = Nothing
   toArity f = Unary (\x -> f [x])
   liftIso p (Unary f) = Unary (from p . f . to p)

instance Arity Binary where
   listify (Binary f) [x, y] = Just (f x y)
   listify _ _ = Nothing
   toArity f = Binary (\x y -> f [x, y])
   liftIso p (Binary f) = Binary (\x y -> from p (f (to p x) (to p y)))

instance Arity Nary where
   listify (Nary f) = Just . f
   toArity = Nary
   liftIso p (Nary f) = Nary (from p . f . map (to p))