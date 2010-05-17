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

import Common.Transformation
import Common.Context
import qualified Service.Diagnose as Diagnose
import Service.Diagnose (Diagnosis, diagnose)
import Service.TypedAbstractService
import Service.Types hiding (State)
import qualified Service.Types as Tp

-- Note that in the typed setting there is no syntax error
data Result a = Buggy  [Rule (Context a)]   
              | NotEquivalent      
              | Ok     [Rule (Context a)] (State a)  -- equivalent
              | Detour [Rule (Context a)] (State a)  -- equivalent
              | Unknown                   (State a)  -- equivalent
 
fromDiagnose :: Diagnosis a -> Result a
fromDiagnose diagnose =
   case diagnose of
      Diagnose.Buggy r        -> Buggy [r]
      Diagnose.NotEquivalent  -> NotEquivalent
      Diagnose.Similar _ s    -> Ok [] s
      Diagnose.Expected _ s r -> Ok [r] s
      Diagnose.Detour _ s r   -> Detour [r] s
      Diagnose.Correct _ s    -> Unknown s
          
submit :: State a -> a -> Result a 
submit state new = fromDiagnose (diagnose state new)
   
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
submitTypeSynonym = typeSynonym "Result" to from tp
 where
   to (Left rs) = Buggy rs
   to (Right (Left ())) = NotEquivalent
   to (Right (Right (Left (rs, s)))) = Ok rs s
   to (Right (Right (Right (Left (rs, s))))) = Detour rs s
   to (Right (Right (Right (Right s)))) = Unknown s

   from (Buggy rs)      = Left rs
   from (NotEquivalent) = Right (Left ())
   from (Ok rs s)       = Right (Right (Left (rs, s)))
   from (Detour rs s)   = Right (Right (Right (Left (rs, s))))
   from (Unknown s)     = Right (Right (Right (Right s))) 

   tp  =  List Rule 
      :|: Unit
      :|: Pair (List Rule) Tp.State
      :|: Pair (List Rule) Tp.State
      :|: Tp.State