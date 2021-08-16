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
-----------------------------------------------------------------------------

module Ideas.Service.Apply
   ( apply
   ) where

import Data.List
import Data.Maybe
import Ideas.Common.Library hiding (apply)
import Ideas.Service.State
import Ideas.Service.BasicServices (allfirsts)
import qualified Data.Set as S

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Rule (Context a) -> Location -> Environment -> State a -> Either String (State a)
apply r loc env state
   | withoutPrefix state = applyOff
   | otherwise           = applyOn
 where
   applyOn = -- scenario 1: on-strategy
      maybe applyOff Right $ listToMaybe
      [ s1 | xs <- either (const []) return (allfirsts state), ((r1, loc1, env1), s1) <- xs, r == r1, loc == loc1, noBindings env || env == env1 ]

   ca = setLocation loc (stateContext state)
   applyOff  = -- scenario 2: off-strategy
      case transApplyWith env (transformation r) ca of
         (new, _):_ -> Right (restart (state {stateContext = new, statePrefix = noPrefix}))
         [] ->
            -- first check the environment (exercise-specific property)
            case environmentCheck of
               Just msg ->
                  Left msg
               Nothing ->
                  -- try to find a buggy rule
                  case siblingsFirst [ (br, envOut) | br <- ruleset (exercise state), isBuggy br,  (_, envOut) <- transApplyWith env (transformation br) ca ] of
                     []  -> Left ("Cannot apply " ++ show r)
                     brs -> Left ("Buggy rule " ++ intercalate "+" (map pp brs))
    where
      pp (br, envOut)
         | noBindings envOut = show br
         | otherwise         = show br ++ " {" ++ show envOut ++ "}"

   siblingsFirst xs = ys ++ zs
    where
      (ys, zs) = partition (siblingInCommon r . fst) xs

   environmentCheck :: Maybe String
   environmentCheck = do
      p <- getProperty "environment-check" (exercise state)
      p env

siblingInCommon :: Rule a -> Rule a -> Bool
siblingInCommon r1 r2 = not (S.null (getSiblings r1 `S.intersection` getSiblings r2))
 where
   getSiblings r = S.fromList (getId r : ruleSiblings r)
