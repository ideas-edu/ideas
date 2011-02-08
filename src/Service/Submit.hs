-----------------------------------------------------------------------------
-- Copyright 2010, Open Universiteit Nederland. This file is distributed 
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
module Service.Submit 
   ( submit, Result(..), getResultState
   , submitType, submitTypeSynonym
   ) where

import Common.Library
import qualified Service.Diagnose as Diagnose
import Service.Diagnose (Diagnosis, diagnose)
import Service.State
import Service.Types

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]   
              | NotEquivalent      
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent
 
fromDiagnose :: Diagnosis a -> Result a
fromDiagnose diagnosis =
   case diagnosis of
      Diagnose.Buggy r        -> Buggy [r]
      Diagnose.NotEquivalent  -> NotEquivalent
      Diagnose.Similar _ s    -> Ok [] s
      Diagnose.Expected _ s r -> Ok [r] s
      Diagnose.Detour _ s r   -> Detour [r] s
      Diagnose.Correct _ s    -> Unknown s
          
submit :: State a -> a -> Result a 
submit state = fromDiagnose . diagnose state
   
getResultState :: Result a -> Maybe (State a)
getResultState result =
   case result of
      Ok _ st     -> return st
      Detour _ st -> return st
      Unknown st  -> return st
      _           -> Nothing
      
submitType :: Type a (Result a)
submitType = useSynonym submitTypeSynonym

submitTypeSynonym :: TypeSynonym a (Result a)
submitTypeSynonym = typeSynonym "Result" f g tp
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

   tp  =  List Rule 
      :|: Unit
      :|: Pair (List Rule) stateTp
      :|: Pair (List Rule) stateTp
      :|: stateTp