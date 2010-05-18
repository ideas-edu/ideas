{-# OPTIONS -XRankNTypes #-}
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
import Service.TypedExample
import Service.Types
import Service.DomainReasoner
import Service.State
import Text.HTML
import qualified Text.XML as XML
import Text.XML (XML)
import Control.Monad
import Common.Exercise
import Common.Utils (Some(..))

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
            highlightXML True xmlRequest
            bold $ text "Reply:"
            highlightXML True xmlReply
            unless xmlTest $ 
               XML.element "font" $ do
                  "color" XML..=. "red"
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
   logic1 pkg = newState pkg "~(p /\\ ~q)"
   logic2 pkg = newState pkg "~~p /\\ T"
   
   lineq1, lineq2 :: Args
   lineq1 pkg = newState pkg "5*(x+1) == 11"
   lineq2 pkg = newState pkg "5*(x+1) == (x-1)/2"
   
   (f +++ g) pkg = f pkg ++ g pkg
   
   noCfg _    = [Nothing ::: maybeTp StrategyCfg]
   noArgs _   = []
   exArgs pkg = [pkg ::: ExercisePkg]

tryAll :: [DomainReasoner a] -> DomainReasoner [a]
tryAll xs = 
   let f m = liftM return m `catchError` const (return [])
   in liftM concat (mapM f xs)
      
newState :: Monad m => ExercisePackage a -> String -> m (TypedValue a)
newState pkg s = do
   let ex = exercise pkg
   case parser ex s of
      Left msg -> fail ("newState: " ++ msg)
      Right a  -> return (emptyState pkg a ::: stateTp)
      
type Args = forall a . ExercisePackage a -> [TypedValue a]

makeExample :: String -> Args -> String -> DomainReasoner Example
makeExample pkgName f srvName = do
   Some pkg <- case readCode pkgName of
                  Just pkg -> findPackage pkg
                  Nothing  -> fail "unknown package name"
   let ex  = exercise pkg
       msg = show (exerciseCode ex)
   srv <- findService srvName
   tr  <- typedExample pkg srv (f pkg)
   return (msg, tr)