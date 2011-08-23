{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- Copyright 2011, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-----------------------------------------------------------------------------
module Documentation.ServicePage (makeServicePage) where

import Common.Exercise
import Common.Id
import Common.Utils (Some(..))
import Control.Monad
import Documentation.DefaultPage
import Service.DomainReasoner
import Service.State
import Service.TypedExample
import Service.Types
import Text.HTML
import Text.XML (XML)

makeServicePage :: String -> Service -> DomainReasoner ()
makeServicePage dir s = do
   xs <- examplesFor (showId s)
   generatePageAt 1 dir (servicePageFile s)  (servicePage xs s)

servicePage :: [Example] -> Service -> HTMLBuilder
servicePage xs s = do
   h1 (showId s)

   para $ do
      bold $ text "Signature:"
      space
      case serviceFunction s of
         _ ::: t -> ttText (show t)
   para $ do
      bold $ text "Description: "
      br
      text $ description s

   when (serviceDeprecated s) $
      para $ bold $ text "Warning: this service is deprecated!"

   unless (null xs) $ do
      h2 $ "XML examples (" ++ show (length xs) ++ ")"
      forM_ (zip [1::Int ..] xs) $
         \(i, (msg, (xmlRequest, xmlReply, xmlTest))) -> do
            h2 $ show i ++ ". " ++ msg
            bold $ text "Request:"
            highlightXML True xmlRequest
            bold $ text "Reply:"
            highlightXML True xmlReply
            unless xmlTest $
               spanClass "error" $
                  bold $ text "Error: invalid request/reply pair"

-----------------------------------------------------------------------
-- Examples

type Example = (String, (XML, XML, Bool))

examplesFor :: String -> DomainReasoner [Example]
examplesFor s = tryAll [ f t | (t, f) <- list, s == t ]
 where
   list =
      [ ("derivation",   makeExample "logic.dnf"  (noCfg +++ logic1))
      , ("derivation",   makeExample "math.lineq" (noCfg +++ lineq1))
      , ("allfirsts",    makeExample "logic.dnf"  logic2)
      , ("allfirsts",    makeExample "math.lineq" lineq2)
      , ("onefirst",     makeExample "logic.dnf"  logic2)
      , ("onefirst",     makeExample "math.lineq" lineq2)
      , ("rulesinfo",    makeExample "math.lineq" noArgs)
      , ("rulelist",     makeExample "math.lineq" exArgs)
      , ("strategyinfo", makeExample "math.lineq" exArgs)
      , ("examples",     makeExample "math.lineq" exArgs)
      ]

   logic1, logic2 :: Args
   logic1 ex = newState ex "~(p /\\ ~q)"
   logic2 ex = newState ex "~~p /\\ T"

   lineq1, lineq2 :: Args
   lineq1 ex = newState ex "5*(x+1) == 11"
   lineq2 ex = newState ex "5*(x+1) == (x-1)/2"

   (f +++ g) ex = f ex ++ g ex

   noCfg _   = [Nothing ::: maybeType StrategyCfg]
   noArgs _  = []
   exArgs ex = [ex ::: Exercise]

tryAll :: [DomainReasoner a] -> DomainReasoner [a]
tryAll xs =
   let f m = liftM return m `catchError` const (return [])
   in liftM concat (mapM f xs)

newState :: Monad m => Exercise a -> String -> m (TypedValue a)
newState ex s =
   case parser ex s of
      Left msg -> fail ("newState: " ++ msg)
      Right a  -> return (emptyState ex a ::: stateType)

type Args = forall a . Exercise a -> [TypedValue a]

makeExample :: String -> Args -> String -> DomainReasoner Example
makeExample exName f srvName = do
   Some ex <- findExercise (newId exName)
   srv     <- findService srvName
   tr      <- typedExample ex srv (f ex)
   return (showId ex, tr)