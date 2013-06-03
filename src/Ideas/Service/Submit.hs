{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- Copyright 2013, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
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
   ( submit, Result(..)
   ) where

import Ideas.Common.Library
import Ideas.Service.Diagnose (Diagnosis, diagnose)
import Ideas.Service.State
import Ideas.Service.Types
import qualified Ideas.Service.Diagnose as Diagnose

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]
              | NotEquivalent
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent

fromDiagnose :: Diagnosis a -> Result a
fromDiagnose diagnosis =
   case diagnosis of
      Diagnose.Buggy _ r       -> Buggy [r]
      Diagnose.NotEquivalent   -> NotEquivalent
      Diagnose.Similar _ s     -> Ok [] s
      Diagnose.Expected _ s r  -> Ok [r] s
      Diagnose.Detour _ s _ r  -> Detour [r] s
      Diagnose.Correct _ s     -> Unknown s
--      Diagnose.Missing         -> NotEquivalent
--      Diagnose.IncorrectPart _ -> NotEquivalent

submit :: State a -> Context a -> Result a
submit state = fromDiagnose . diagnose state

instance Typed a (Result a) where
   typed = Tag "Result" (Iso (f <-> g) typed)
    where
      f (Left rs) = Buggy rs
      f (Right (Left ())) = NotEquivalent
      f (Right (Right (Left (rs, s)))) = Ok rs s
      f (Right (Right (Right (Left (rs, s))))) = Detour rs s
      f (Right (Right (Right (Right s)))) = Unknown s

      g (Buggy rs)      = Left rs
      g (NotEquivalent) = Right (Left ())
      g (Ok rs s)       = Right (Right (Left (rs, s)))
      g (Detour rs s)   = Right (Right (Right (Left (rs, s))))
      g (Unknown s)     = Right (Right (Right (Right s)))