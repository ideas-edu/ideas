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
  , Result(..), violation, relevance
  , subConstraints
  ) where

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
      Violation s -> Just s
      _           -> Nothing

---------------------------------------------------------------------------
-- Result

data Result a = Irrelevant | Violation String | Ok a
   deriving Show

instance Functor Result where
   fmap _ Irrelevant      = Irrelevant
   fmap _ (Violation msg) = Violation msg
   fmap f (Ok a)          = Ok (f a)

instance Applicative Result where
   pure = Ok
   Irrelevant     <*> _             = Irrelevant
   Violation msg  <*> _             = Violation msg
   Ok _           <*> Irrelevant    = Irrelevant
   Ok _           <*> Violation msg = Violation msg
   Ok f           <*> Ok a          = Ok (f a)

instance Monad Result where
   Irrelevant    >>= _ = Irrelevant
   Violation msg >>= _ = Violation msg
   Ok a          >>= f = f a

violation :: String -> Result a
violation = Violation

-- | Turn errors into irrelevant results
relevance :: Result a -> Result a
relevance (Violation _) = Irrelevant
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
      errs -> violation $ intercalate "," [ showId c ++ "." ++ loc ++ ":" ++ msg | (loc, Violation msg) <- errs ]
 where
   isError (_, Violation _) = True
   isError _ = False

   isOk (_, Ok _) = True
   isOk _ = False
