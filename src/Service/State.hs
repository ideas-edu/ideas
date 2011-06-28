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
     State, makeState, empyStateContext, emptyState 
   , exercise, statePrefix, stateContext, stateTerm
     -- * Types
   , stateType
   ) where

import Common.Library
import Common.Utils (readM)
import Data.Maybe
import Service.Types

data State a = State 
   { exercise     :: Exercise a
   , statePrefix  :: Maybe (Prefix (Context a))
   , stateContext :: Context a
   }
   
instance Show (State a) where
   show s = unlines $ "State {" : map ("   "++) xs ++ ["}"]
    where
      xs = [ "exercise = " ++ showId s
           , "prefix   = " ++ maybe "no prefix" show (statePrefix s)
           , "steps    = " ++ maybe "no prefix" (show . prefixToSteps) (statePrefix s)
           , "term     = " ++ prettyPrinterContext (exercise s) (stateContext s)
           ]

instance HasId (State a) where 
   getId = getId . exercise
   changeId f s = s { exercise = changeId f (exercise s) }

stateTerm :: State a -> a
stateTerm = fromMaybe (error "invalid term") . fromContext . stateContext

-----------------------------------------------------------

makeState :: Exercise a -> Maybe (Prefix (Context a)) -> Context a -> State a
makeState = State

empyStateContext :: Exercise a -> Context a -> State a
empyStateContext ex = makeState ex (Just pr)
 where
   pr = emptyPrefix (strategy ex)

emptyState :: Exercise a -> a -> State a
emptyState ex = empyStateContext ex . inContext ex

--------------------------------------------------------------

stateType :: Type a (State a)
stateType = Tag "state" (Iso (f <-> g) tp)
 where
   f (ex, mp, ctx) =
      let str = strategy ex
          h   = fromMaybe [] . readM
      in makeState ex (mp >>= flip makePrefix str . h) ctx
   g st = 
      ( exercise st
      , fmap show (statePrefix st)
      , stateContext st
      )
   tp = tuple3 Exercise prefixType Context

   -- iso prevents that prefix is turned into an (XML) attribute
   prefixType = maybeType (Tag "prefix" (Iso identity String))