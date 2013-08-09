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
-- The information maintained for a learner trying to complete a
-- derivation.
--
-----------------------------------------------------------------------------
module Ideas.Service.State
   ( -- * Exercise state
     State, makeState, makeNoState, emptyStateContext, emptyState
   , exercise, statePrefixes, stateContext, stateTerm, stateLabels
   ) where

import Data.List
import Data.Maybe
import Ideas.Common.Library
import Ideas.Common.Strategy.Abstract (LabelInfo)

data State a = State
   { exercise      :: Exercise a
   , statePrefixes :: [Prefix (Context a)]
   , stateContext  :: Context a
   }

instance Show (State a) where
   show s = unlines $ "State {" : map ("   "++) xs ++ ["}"]
    where
      xs = [ "exercise = " ++ showId s
           , "prefix   = " ++ intercalate ";" (map showPrefix (statePrefixes s))
           , "steps    = " ++ intercalate ";" (map (show . prefixToSteps) (statePrefixes s))
           , "term     = " ++ prettyPrinterContext (exercise s) (stateContext s)
           ]

instance HasId (State a) where
   getId = getId . exercise
   changeId f s = s { exercise = changeId f (exercise s) }

instance HasEnvironment (State a) where
   environment = environment . stateContext
   setEnvironment env s =
      s { stateContext = setEnvironment env (stateContext s) }

stateTerm :: State a -> a
stateTerm = fromMaybe (error "invalid term") . fromContext . stateContext

stateLabels :: State a -> [[LabelInfo]]
stateLabels state =
    map (filterRules . activeLabels) $ statePrefixes state
  where
    rs          = ruleset $ exercise state
    isRule      = flip elem (map getId rs) . getId
    filterRules = filter (not . isRule)

-----------------------------------------------------------

makeState :: Exercise a -> [Prefix (Context a)] -> Context a -> State a
makeState = State

-- State without a prefix
makeNoState :: Exercise a -> Context a -> State a
makeNoState = flip makeState []

emptyStateContext :: Exercise a -> Context a -> State a
emptyStateContext ex ca = 
   let pr = emptyPrefix (strategy ex) ca
   in makeState ex [pr] ca

emptyState :: Exercise a -> a -> State a
emptyState ex = emptyStateContext ex . inContext ex