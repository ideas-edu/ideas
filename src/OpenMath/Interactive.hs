-----------------------------------------------------------------------------
-- Copyright 2008, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  bastiaan.heeren@ou.nl
-- Stability   :  provisional
-- Portability :  portable (depends on ghc)
--
-- (...add description...)
--
-----------------------------------------------------------------------------
module OpenMath.Interactive (respondHTML, oneliner) where

import Common.Exercise
import Service.ExerciseList
import Common.Transformation
import Common.Strategy hiding (not)
import OpenMath.LAServer
import OpenMath.StrategyTable
import OpenMath.Conversion
import OpenMath.Request
import OpenMath.Reply
import Service.HTML
import Data.Char
import Data.List
import Data.Maybe

respondHTML :: String -> String -> String
respondHTML self = either (const "") (showHTML . makeHTML self) . pRequest
           
makeHTML :: String -> Request -> HTML
makeHTML self req = undefined {-
   case getOpenMathExercise (req_Code req) of
      Just omex -> 
         case laServerFor omex noAnswer of
            Incorrect inc -> make self omex noAnswer inc
            _ -> errorPage "request error: invalid request"
      _ -> errorPage "request error: ambiguous strategy"      
 where
   noAnswer = req {req_Answer = Nothing}
               
make :: String -> OpenMathExercise -> Request -> ReplyIncorrect -> HTML
make self omex@(OMEX a) req inc = htmlPage title $ do
   para $ do 
      bold (text "Term:  ")
      preText (maybe "" (prettyPrinter a) $ getTerm req) 
      
   para $ do 
      bold (text "Expected: ")
      preText (maybe "" (prettyPrinter a) $ fromOMOBJ $ repInc_Expected inc) 
          
   para $ do 
      text "Submit the "
      link (reqToURL reqOk) (text "expected")
      text " answer and continue"
   
   hr
   para $ do
      bold (text "Strategy: ") >> text (show $ req_Code req) >> br
      bold (text "Steps remaining: ") >> text (show (repInc_Steps inc) ++ " (and " ++ show n ++ " after submitting the expected answer)") >> br
      bold (text "Arguments: ") >> text (formatArgs $ repInc_Arguments inc) >> br
      bold (text "Location: ") >> text (show $ req_Location req) 
   
   let f = showLoc (req_Location req)
   para $ pre $ mapM_ f $ strategyLocations $ strategy a

   para $ do 
      link (reqToURL reqZoomIn)  (text "Zoom in")  >> text " to a substrategy or " 
      link (reqToURL reqZoomOut) (text "zoom out") >> text " to the parent strategy" 
          
   hr
   para $ do 
      bold (text "Context: ") >> text (fromMaybe "" $ req_Context req)
      text "Remove all " >> link (reqToURL reqNoCtxt) (text " context information")
   
   para $ do 
      bold (text "Derivation (so far): ") >> text derivation

   let f (x, y) = text x >> preText (maybe "" (prettyPrinter a) (fromOMOBJ y))
       len = length (repInc_Derivation inc)
   para $ do
      bold (text $ "Derivation (expected, " ++ show len ++ " steps):")
      ul (map f $ repInc_Derivation inc)
      preText $ concat $ map show $ repInc_Derivation inc
      
 where
   title      = "LA Feedback Service (version " ++ versionNr ++ ")"
   (reqOk, n) = expected omex req inc
   reqZoomIn  = zoomIn req inc
   reqZoomOut = zoomOut req
   reqNoCtxt  = removeContext req
   reqToURL   = (self++) . oneliner . ppRequest
   formatArgs = concat . intersperse ", " . map (\(a, b) -> a ++ " = " ++ b)
   derivation = case prefixToSteps (getPrefix req (strategy a)) of
                   steps -> concat $ intersperse "; " [ name r | Step _ r <- steps, isMajorRule r ] 

----------------------------------------------------------------------------
-- Actions

expected :: OpenMathExercise -> Request -> ReplyIncorrect -> (Request, Int)
expected omex r inc = 
   case laServerFor omex r {req_Answer = Just $ repInc_Expected inc} of
      Ok ok -> ( r { req_Location = repOk_Location ok
                   , req_Term     = repInc_Expected inc
                   , req_Context  = Just (repOk_Context ok)
                   }, repOk_Steps ok )
      _ -> (r, 0)-}

zoomIn :: Request -> ReplyIncorrect -> Request
zoomIn r inc = r { req_Location = repInc_Location inc }

zoomOut :: Request -> Request
zoomOut r = r { req_Location = case req_Location r of
                                  [] -> []
                                  xs -> init xs }

removeContext :: Request -> Request
removeContext r = r { req_Context = Nothing }

----------------------------------------------------------------------------

showLoc :: StrategyLocation -> (StrategyLocation, StrategyOrRule a) -> HTMLBuilder
showLoc here (loc, eitherValue) 
   | loc `isPrefixOf` here = do
        text (replicate (length loc*2) '.')
        bold (text txt)
        br
   | not (null loc) && init loc `isPrefixOf` here && init loc /= here = do
        text (replicate (length loc*2) '.' ++ txt)
        br
   | otherwise = return ()
 where
   txt = either strategyName ((++" (rule)") . name) eitherValue
 
-- imgOUNL :: XML
-- imgOUNL = Tag "img" [("border","0"),("src","ounl.jpg"),("align","right"),("alt","OUNL")] []