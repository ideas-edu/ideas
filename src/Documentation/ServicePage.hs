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
-----------------------------------------------------------------------------
module Documentation.ServicePage (makeServicePage) where

import Documentation.DefaultPage
import Service.ExercisePackage
import Service.ServiceList
import Service.TypedExample
import Service.Types
import Service.DomainReasoner
import Service.TypedAbstractService (emptyState)
import Text.HTML
import qualified Text.XML as XML
import Text.XML (XML, showXML)
import Domain.Logic
import Domain.Math.Polynomial.Exercises
import Domain.Math.Data.Relation
import Domain.Math.Expr.Symbolic
import Control.Monad
import Common.Utils (ShowString(..))

makeServicePage :: String -> Service -> DomainReasoner ()
makeServicePage dir s = do
   xs <- examplesFor (serviceName s)
   generatePageAt 1 dir (servicePageFile s)  (servicePage xs s)

servicePage :: [Example] -> Service -> HTMLBuilder
servicePage examples s = do
   h1 (serviceName s)

   para $ do
      bold $ text "Signature:"
      space
      case serviceFunction s of
         _ ::: t -> ttText (show t)
   para $ do
      bold $ text "Description: "
      br
      text $ serviceDescription s

   when (serviceDeprecated s) $ 
      para $ bold $ text "Warning: this service is deprecated!"
   
   unless (null examples) $ do
      h2 $ "XML examples (" ++ show (length examples) ++ ")"
      forM_ (zip [1..] examples) $ 
         \(i, (msg, (xmlRequest, xmlReply, xmlTest))) -> do
            h2 $ show i ++ ". " ++ msg
            bold $ text "Request:"
            preText $ showXML xmlRequest
            bold $ text "Reply:"
            preText $ showXML xmlReply
            unless xmlTest $ 
               XML.element "font" $ do
                  "color" XML..=. "red"
                  bold $ text "Error: invalid request/reply pair"

-----------------------------------------------------------------------
-- Examples

type Example = (String, (XML, XML, Bool))

examplesFor :: String -> DomainReasoner [Example]
examplesFor s = sequence [ m | (t, m) <- list, s == t ]
 where
   list = 
      [ logic "derivation" [Nothing ::: Maybe StrategyCfg, stLogic1]
      , lineq "derivation" [Nothing ::: Maybe StrategyCfg, stLineq1]
      , logic "allfirsts" [stLogic2]
      , lineq "allfirsts" [stLineq2]
      , logic "onefirst" [stLogic2]
      , lineq "onefirst" [stLineq2]
--      , logic "applicable" [[] ::: Location, stLogic1]
      , lineq "rulesinfo" []
      , lineq "rulelist" [linearExercise ::: Exercise]
      , lineq "strategyinfo" [linearExercise ::: Exercise]
      ]
   strVar   = Var . ShowString
   stLogic1 = emptyState dnfExercise (Not (strVar "p" :&&: Not (strVar "q"))) ::: State
   stLogic2 = emptyState dnfExercise (Not (Not (strVar "p")) :&&: Not T) ::: State
   stLineq1 = emptyState linearExercise (5*(variable "x"+1) :==: 11) ::: State
   stLineq2 = emptyState linearExercise (5*(variable "x"+1) :==: (variable "x"-1)/2) ::: State
   
   logic = make "Logic" (package dnfExercise)
   lineq = make "Linear equation" (termPackage linearExercise)
      
   make msg pkg fs args = (fs, do
      srv <- findService fs
      tr  <- typedExample pkg srv args
      return (msg, tr))