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
-- The information maintained for a learner trying to complete a
-- derivation.
--
-----------------------------------------------------------------------------
module Service.State 
   ( -- * Exercise state
     State(..), emptyState, term
     -- * Types
   , stateTp, stateTypeSynonym
   ) where

import Common.Context
import Common.Exercise
import Common.Strategy
import Common.Utils (readM)
import Service.Types
import Data.Maybe
import qualified Service.ExercisePackage as Pkg

data State a = State 
   { exercisePkg :: Pkg.ExercisePackage a
   , prefix      :: Maybe (Prefix (Context a))
   , context     :: Context a
   }

term :: State a -> a
term = fromMaybe (error "invalid term") . fromContext . context

-----------------------------------------------------------

emptyState :: Pkg.ExercisePackage a -> a -> State a
emptyState pkg a = State
   { exercisePkg = pkg
   , prefix      = Just (emptyPrefix (strategy ex))
   , context     = inContext ex a
   }
 where
   ex = Pkg.exercise pkg
   
--------------------------------------------------------------

stateTp :: Type a (State a)
stateTp = useSynonym stateTypeSynonym

stateTypeSynonym :: TypeSynonym a (State a)
stateTypeSynonym = typeSynonym "State" to from tp
 where
   to (pkg, mp, ctx) =
      let str = strategy (Pkg.exercise pkg)
          f   = fromMaybe [] . readM
      in State pkg (mp >>= flip makePrefix str . f) ctx
   from st = 
      ( exercisePkg st
      , fmap show (prefix st)
      , context st
      )
   tp = tuple3 ExercisePkg prefixTp Context

prefixTp :: Type a (Maybe String)
prefixTp = Tag "Prefix" (maybeTp String)