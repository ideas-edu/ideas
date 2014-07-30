{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
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
--  $Id$

module Ideas.Service.State
   ( -- * Exercise state
     State, makeState, makeNoState, emptyStateContext, emptyState
   , exercise, statePrefix, stateContext, stateTerm
   , withoutPrefix, stateLabels, finished, firsts
   ) where

import Data.Function
import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (ready, (:~>))
import Ideas.Common.Strategy.Sequence

data State a = State
   { exercise     :: Exercise a
   , statePrefix  :: Prefix (Context a)
   , stateContext :: Context a
   }

instance Show (State a) where
   show s = unlines $ "State {" : map ("   "++) xs ++ ["}"]
    where
      xs = [ "exercise = " ++ showId s
           , "prefix   = " ++ show (statePrefix s)
           , "term     = " ++ prettyPrinterContext (exercise s) (stateContext s)
           ]

instance HasId (State a) where
   getId = getId . exercise
   changeId f s = s { exercise = changeId f (exercise s) }

instance HasEnvironment (State a) where
   environment = environment . stateContext
   setEnvironment env s =
      s { stateContext = setEnvironment env (stateContext s) }

instance Firsts (State a) where 
   type Elem (State a) = (Step (Context a), Context a)

   firsts st = firstsOrdered cmp st
    where
      cmp = ruleOrdering (exercise st) `on` (f . fst)
      
      f (RuleStep _ r) = r
      f _ = emptyRule ()

   menu st = fmap f (menu (majorPrefix (statePrefix st)))
    where
      f Done = Done
      f (info :~> p) = info :~> State (exercise st) p (snd info)

stateTerm :: State a -> a
stateTerm = fromMaybe (error "invalid term") . fromContext . stateContext

-----------------------------------------------------------

makeState :: Exercise a -> Prefix (Context a) -> Context a -> State a
makeState = State

-- State without a prefix
makeNoState :: Exercise a -> Context a -> State a
makeNoState = flip makeState noPrefix

emptyStateContext :: Exercise a -> Context a -> State a
emptyStateContext ex ca =
   let pr = emptyPrefix (strategy ex) ca
   in makeState ex pr ca

emptyState :: Exercise a -> a -> State a
emptyState ex = emptyStateContext ex . inContext ex

withoutPrefix :: State a -> Bool
withoutPrefix = null . prefixPaths . statePrefix

finished :: State a -> Bool
finished st = isReady (exercise st) (stateTerm st)

stateLabels :: State a -> [[Id]]
stateLabels st = map make (prefixPaths (statePrefix st))
 where
   ex = exercise st
   make path = 
      case replayPath path (strategy ex) (stateContext st) of 
         Just (xs, _) -> nub [l | Enter l <- xs] \\ [l | Exit l <- xs]
         Nothing -> []
   