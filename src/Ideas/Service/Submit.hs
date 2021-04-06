{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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
-- Diagnose a term submitted by a student. Deprecated (see diagnose service).
--
-----------------------------------------------------------------------------

module Ideas.Service.Submit
   ( submit, Result(..), tResult, getState
   ) where

import Data.Maybe
import Ideas.Common.Library
import Ideas.Service.Diagnose (Diagnosis, diagnose)
import Ideas.Service.State
import Ideas.Service.Types
import qualified Ideas.Service.Diagnose as Diagnose

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]
              | NotEquivalent String
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent

getState :: Result a -> Maybe (State a)
getState r =
   case r of
      Buggy _         -> Nothing
      NotEquivalent _ -> Nothing
      Ok _ s          -> Just s
      Detour _ s      -> Just s
      Unknown s       -> Just s

fromDiagnose :: Diagnosis a -> Result a
fromDiagnose diagnosis =
   case diagnosis of
      Diagnose.SyntaxError s    -> NotEquivalent s -- should not happen
      Diagnose.Buggy _ r        -> Buggy [r]
      Diagnose.NotEquivalent s  -> NotEquivalent s
      Diagnose.Similar _ s _    -> Ok [] s
      Diagnose.Expected _ s r   -> Ok [r] s
      Diagnose.WrongRule _ s mr -> Ok (maybeToList mr) s
      Diagnose.Detour _ s _ r   -> Detour [r] s
      Diagnose.Correct _ s      -> Unknown s
      Diagnose.Unknown _ s      -> Unknown s
--      Diagnose.Missing         -> NotEquivalent
--      Diagnose.IncorrectPart _ -> NotEquivalent

submit :: State a -> Context a -> Result a
submit state ctx = fromDiagnose (diagnose state ctx Nothing)

tResult :: Type a (Result a)
tResult = Tag "Result" (Iso (f <-> g) tp)
    where
      tp = tList tRule :|: tString :|: tPair (tList tRule) tState
           :|: tPair (tList tRule) tState :|: tState

      f (Left rs) = Buggy rs
      f (Right (Left s)) = NotEquivalent s
      f (Right (Right (Left (rs, s)))) = Ok rs s
      f (Right (Right (Right (Left (rs, s))))) = Detour rs s
      f (Right (Right (Right (Right s)))) = Unknown s

      g (Buggy rs)        = Left rs
      g (NotEquivalent s) = Right (Left s)
      g (Ok rs s)         = Right (Right (Left (rs, s)))
      g (Detour rs s)     = Right (Right (Right (Left (rs, s))))
      g (Unknown s)       = Right (Right (Right (Right s)))