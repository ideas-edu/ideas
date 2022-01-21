-----------------------------------------------------------------------------
-- Copyright 2019, Ideas project team. This file is distributed under the
-- terms of the Apache License 2.0. For more information, see the files
-- "LICENSE.txt" and "NOTICE.txt", which are included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- Constraints for constraint-based tutors
--
-----------------------------------------------------------------------------

module Ideas.Common.Constraint
  ( Constraint, makeConstraint
  , isRelevant, isSatisfied, isViolated, getResult
  , Result(..), relevance
  , subConstraints
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Ideas.Common.Id
import Ideas.Common.View

---------------------------------------------------------------------------
-- Constraint

data Constraint a = C
  { constraintId :: Id
  , getResult    :: a -> Result ()
  }

instance Show (Constraint a) where
  show = showId

instance Eq (Constraint a) where
  r1 == r2 = constraintId r1 == constraintId r2

instance Ord (Constraint a) where
  compare = compareId

instance HasId (Constraint a) where
  getId        = constraintId
  changeId f r = r { constraintId = f (constraintId r) }

instance Lift Constraint where
   liftWithM f (C n p) = C n (maybe Irrelevant (p . fst) . f)

makeConstraint :: IsId n => n -> (a -> Result ()) -> Constraint a
makeConstraint = C . newId

--  | Relevance condition
isRelevant :: Constraint a -> a -> Bool
isRelevant p a =
   case getResult p a of
      Irrelevant -> False
      _          -> True

-- | Satisfaction condition
isSatisfied :: Constraint a -> a -> Bool
isSatisfied p a =
   case getResult p a of
      Ok _ -> True
      _    -> False

-- | Satisfaction condition
isViolated :: Constraint a -> a -> Maybe String
isViolated p a =
   case getResult p a of
      Error s -> Just s
      _       -> Nothing

---------------------------------------------------------------------------
-- Result

data Result a = Irrelevant | Error String | Ok a
   deriving Show

instance Functor Result where
   fmap _ Irrelevant  = Irrelevant
   fmap _ (Error msg) = Error msg
   fmap f (Ok a)      = Ok (f a)

instance Applicative Result where
   pure = Ok
   Irrelevant <*> _          = Irrelevant
   Error msg  <*> _          = Error msg
   Ok _       <*> Irrelevant = Irrelevant
   Ok _       <*> Error msg  = Error msg
   Ok f       <*> Ok a       = Ok (f a)

instance Alternative Result where
   empty = Error ""
   Irrelevant <|> r       = r
   Error msg  <|> Error _ = Error msg -- left-biased
   Error _    <|> r       = r
   Ok a       <|> _       = Ok a

instance Monad Result where
   return = Ok
   fail   = Error
   Irrelevant >>= _ = Irrelevant
   Error msg  >>= _ = Error msg
   Ok a       >>= f = f a

instance MonadPlus Result where
   mzero = empty
   mplus = (<|>)

-- | Turn errors into irrelevant results
relevance :: Result a -> Result a
relevance (Error _) = Irrelevant
relevance r = r

-- to do:
-- * alle errors teruggeven
-- * locatie van error bijhouden
subConstraints :: IsId n => (b -> [(String, a)]) -> n -> Constraint a -> Constraint b
subConstraints f n c = makeConstraint n $ \p -> do
   let results = [ (loc, getResult c a) | (loc, a) <- f p ]
   case filter isError results of
      [] | any isOk results -> Ok ()
         | otherwise        -> Irrelevant
      errs -> Error $ intercalate "," [ showId c ++ "." ++ loc ++ ":" ++ msg | (loc, Error msg) <- errs ]
 where
   isError (_, Error _) = True
   isError _ = False

   isOk (_, Ok _) = True
   isOk _ = False
