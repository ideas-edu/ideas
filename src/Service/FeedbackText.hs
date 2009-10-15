-----------------------------------------------------------------------------
-- Copyright 2009, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Service.FeedbackText 
   ( feedbackLogic
   , onefirsttext, submittext, derivationtext
   ) where

import Control.Arrow
import Control.Monad
import Common.Exercise
import Common.Utils (safeHead, fst3, commaList)
import Data.Maybe
import Domain.Logic.Formula (SLogic, eqLogic)
import Domain.Logic.FeedbackText
import Domain.Logic.Exercises (dnfExercise, dnfUnicodeExercise)
import Domain.Logic.Difference (difference, differenceEqual)
import Service.TypedAbstractService
import Common.Context
import Common.Exercise
import Common.Transformation (name, Rule)
import Text.Parsing (errorToPositions)
import Data.Char

-- Quick hack for determining subterms
coerceLogic :: Exercise a -> a -> Maybe SLogic
coerceLogic ex a
   | exerciseCode ex == exerciseCode dnfExercise =
        either (const Nothing) Just $ parser dnfExercise (prettyPrinter ex a)
   | exerciseCode ex == exerciseCode dnfUnicodeExercise =
        either (const Nothing) Just $ parser dnfUnicodeExercise (prettyPrinter ex a)
   | otherwise = Nothing

youRewroteInto :: State a -> a -> Maybe String
youRewroteInto = rewriteIntoText False "You rewrote "

useToRewrite :: Rule (Context a) -> State a -> a -> Maybe String
useToRewrite rule old = rewriteIntoText True txt old
 where
   txt = "Use " ++ showRule (exerciseCode $ exercise old) rule
         ++ " to rewrite "

rewriteIntoText :: Bool -> String -> State a -> a -> Maybe String
rewriteIntoText eqOption txt old a = do
   p <- coerceLogic (exercise old) (fromContext $ context old)
   q <- coerceLogic (exercise old) a
   (p1, q1) <- if eqOption then differenceEqual eqLogic p q
                           else difference p q
   let ex | exerciseCode (exercise old) == exerciseCode dnfUnicodeExercise =
               dnfUnicodeExercise
          | otherwise = dnfExercise
   return $ txt ++ prettyPrinter ex p1 
         ++ " into " ++ prettyPrinter ex q1 ++ ". "

-- Feedback messages for submit service (free student input). The boolean
-- indicates whether the student is allowed to continue (True), or forced 
-- to go back to the previous state (False)
feedbackLogic :: State a -> a -> Result a -> (String, Bool)
feedbackLogic old a result =
   case result of
      Buggy rs        -> ( fromMaybe ""  (youRewroteInto old a) ++ 
                           feedbackBuggy (ready old) rs
                         , False)
      NotEquivalent   -> ( fromMaybe ""  (youRewroteInto old a) ++
                           feedbackNotEquivalent (ready old)
                         , False)
      Ok rs _
         | null rs    -> (feedbackSame, False)
         | otherwise  -> feedbackOk rs
      Detour rs _     -> feedbackDetour (ready old) (expected old) rs
      Unknown _       -> ( fromMaybe ""  (youRewroteInto old a) ++ 
                           feedbackUnknown (ready old)
                         , False)
 where
   expected = fmap fst3 . safeHead . allfirsts

showRule :: ExerciseCode -> Rule a -> String
showRule code r 
   | code `elem` map exerciseCode [dnfExercise, dnfUnicodeExercise] =
        fromMaybe txt (ruleText r)
   | otherwise = txt
 where
   txt = "rule " ++ name r

getCode :: State a -> ExerciseCode
getCode = exerciseCode . exercise

derivationtext :: State a -> Maybe String -> [(String, Context a)]
derivationtext st _event = 
   map (first (showRule (getCode st))) (derivation st)

onefirsttext :: State a -> Maybe String -> (Bool, String, State a)
onefirsttext state event =
   case allfirsts state of
      (r, _, s):_ ->
         let msg = case useToRewrite r state (fromContext $ context s) of
                      Just txt | event /= Just "hint button" -> txt
                      _ -> "Use " ++ showRule (getCode state) r
         in (True, msg, s)
      _ -> (False, "Sorry, no hint available", state)

submittext :: State a -> String -> Maybe String -> (Bool, String, State a)
submittext state txt _event = 
   case parser (exercise state) txt of
      Left err -> 
         let msg = "Syntax error" ++ pos ++ ": " ++ show err
             pos = case map show (errorToPositions err) of
                      [] -> ""
                      xs -> " at " ++ commaList xs
         in (False, msg, state)
      Right a  -> 
         let result = submit state a
             (txt, b) = feedbackLogic state a result
         in case getResultState result of
               Just new | b -> (True, txt, resetStateIfNeeded new)
               _ -> (False, txt, state)
