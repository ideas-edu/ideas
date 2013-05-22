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
module Ideas.Documentation.ServicePage (makeServicePage) where

import Ideas.Common.Library hiding ((+++))
import Ideas.Common.Utils (Some(..))
import Control.Monad
import Control.Monad.Error
import Ideas.Documentation.DefaultPage
import Ideas.Service.DomainReasoner
import Ideas.Service.State
import Ideas.Service.TypedExample
import Ideas.Service.Types
import Ideas.Text.HTML
import Ideas.Text.XML (XML)

makeServicePage :: DomainReasoner -> String -> Service -> IO ()
makeServicePage dr dir s = do
   xs <- examplesFor dr (showId s)
   generatePageAt 1 dr dir (servicePageFile s)  (servicePage xs s)

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

examplesFor :: DomainReasoner -> String -> IO [Example]
examplesFor dr s = tryAll [ f t | (t, f) <- list, s == t ]
 where
   list =
      [ ("derivation",   makeExample dr "logic.dnf"  (noCfg +++ logic1))
      , ("derivation",   makeExample dr "math.lineq" (noCfg +++ lineq1))
      , ("allfirsts",    makeExample dr "logic.dnf"  logic2)
      , ("allfirsts",    makeExample dr "math.lineq" lineq2)
      , ("onefirst",     makeExample dr "logic.dnf"  logic2)
      , ("onefirst",     makeExample dr "math.lineq" lineq2)
      , ("rulesinfo",    makeExample dr "math.lineq" noArgs)
      , ("rulelist",     makeExample dr "math.lineq" exArgs)
      , ("strategyinfo", makeExample dr "math.lineq" exArgs)
      , ("examples",     makeExample dr "math.lineq" exArgs)
      ]

   logic1, logic2 :: Args
   logic1 ex = newState ex "~(p /\\ ~q)"
   logic2 ex = newState ex "~~p /\\ T"

   lineq1, lineq2 :: Args
   lineq1 ex = newState ex "5*(x+1) == 11"
   lineq2 ex = newState ex "5*(x+1) == (x-1)/2"

   (f +++ g) ex = f ex ++ g ex

   noCfg _   = [(Nothing :: Maybe StrategyConfiguration) ::: typed]
   noArgs _  = []
   exArgs ex = [ex ::: typed]

tryAll :: [IO a] -> IO [a]
tryAll xs =
   let f m = liftM return m `catchError` const (return [])
   in liftM concat (mapM f xs)

newState :: Monad m => Exercise a -> String -> m (TypedValue (Type a))
newState ex s =
   case parser ex s of
      Left msg -> fail ("newState: " ++ msg)
      Right a  -> return (emptyState ex a ::: typed)

type Args = forall a . Exercise a -> [TypedValue (Type a)]

makeExample :: DomainReasoner -> String -> Args -> String -> IO Example
makeExample dr exName f srvName = do
   Some ex <- findExercise dr (newId exName)
   srv     <- findService dr (newId srvName)
   tr      <- typedExample dr ex srv (f ex)
   return (showId ex, tr) 