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
   ( ApplyResult(..), tApplyResult, apply
   ) where

import Data.Either
import Data.List
import Ideas.Common.Library hiding (apply)
import Ideas.Service.State
import Ideas.Service.BasicServices (allfirsts)
import Ideas.Service.Types
import qualified Data.Set as S

----------------------------------------------------------------
-- Result types for apply service

data ApplyResult a
   = SyntaxError  String
   | Correct      Bool (State a)
   | Buggy        Environment (Rule (Context a))
   | Incorrect

instance Show (ApplyResult a) where
   show (SyntaxError _) = "SyntaxError"
   show (Correct _ _)   = "Correct"
   show (Buggy _ r)     = "Buggy(" ++ show r ++ ")"
   show Incorrect       = "Incorrect"

----------------------------------------------------------------
-- The apply feedback service

-- Two possible scenarios: either I have a prefix and I can return a new one (i.e., still following the
-- strategy), or I return a new term without a prefix. A final scenario is that the rule cannot be applied
-- to the current term at the given location, in which case the request is invalid.
apply :: Rule (Context a) -> Location -> Environment -> State a -> ApplyResult a
apply r loc env st
   | withoutPrefix st = applyOff
   | otherwise        = applyOn
 where
   applyOn = -- scenario 1: on-strategy
      case [ new | (stepInfo, new) <- fromRight [] (allfirsts st), testStep stepInfo ] of
         []    -> applyOff
         new:_ -> correct new

   applyOff  = -- scenario 2: off-strategy
      case transApplyWith env (transformation r) ca of
         (new, _):_ -> correct (restart (st {stateContext = new, statePrefix = noPrefix}))
         [] ->
            -- first check the environment (exercise-specific property)
            case environmentCheck st env of
               Just msg ->
                  SyntaxError msg
               Nothing ->
                  -- try to find a buggy rule
                  case siblingsFirst [ (envOut, br) | br <- ruleset (exercise st), isBuggy br,  (_, envOut) <- transApplyWith env (transformation br) ca ] of
                     []  -> Incorrect
                     (envOut, br):_ -> Buggy envOut br

   ca = setLocation loc (stateContext st)

   siblingsFirst xs = ys ++ zs
    where
      (ys, zs) = partition (siblingInCommon r . snd) xs

   testStep (r1, loc1, env1) =
      r == r1 && loc == loc1 && (noBindings env || env == env1)

environmentCheck :: State a -> Environment -> Maybe String
environmentCheck st env = do
   p <- getProperty "environment-check" (exercise st)
   p env

correct :: State a -> ApplyResult a
correct st = Correct (finished st) st

siblingInCommon :: Rule a -> Rule a -> Bool
siblingInCommon r1 r2 = not (S.null (getSiblings r1 `S.intersection` getSiblings r2))
 where
   getSiblings r = S.fromList (getId r : ruleSiblings r)

----------------------------------------------------------------
-- Helpers

tApplyResult :: Type a (ApplyResult a)
tApplyResult = Tag "ApplyResult" $ Iso (f <-> g) tp
 where
   tp = tString :|: tPair tBool tState :|: tPair tEnvironment tRule :|: tUnit

   f (Left msg)                      = SyntaxError msg
   f (Right (Left (b, st)))          = Correct b st
   f (Right (Right (Left (env, r)))) = Buggy env r
   f (Right (Right (Right ())))      = Incorrect

   g (SyntaxError msg) = Left msg
   g (Correct b st)    = Right (Left (b, st))
   g (Buggy env r)     = Right (Right (Left (env, r)))
   g Incorrect         = Right (Right (Right ()))
