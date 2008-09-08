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
import Common.Transformation
import Common.Strategy hiding (not)
import Common.Utils (Some(..))
import OpenMath.LAServer
import OpenMath.StrategyTable
import OpenMath.Conversion
import OpenMath.Request
import OpenMath.Reply
import Service.XML
import Data.Char
import Data.List
import Data.Maybe

respondHTML :: String -> String -> String
respondHTML self = either (const "") (showXML . makeHTML self) . pRequest

(~=) :: String -> String -> Bool
xs ~= ys = let f = map toLower . filter (not . isSpace)
           in f xs == f ys 
           
makeHTML :: String -> Request -> XML
makeHTML self req = 
   case [ (ea, laServerFor a noAnswer) | Entry _ ea@(Some (ExprExercise a)) _ _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [(Some (ExprExercise a), Incorrect inc)] -> make self a noAnswer inc
      [_] -> Text "request error: invalid request"
      []  -> Text "request error: unknown strategy"
      _   -> Text "request error: ambiguous strategy"
 where
   noAnswer = req {req_Answer = Nothing}
               
make :: IsOMOBJ a => String -> Exercise a -> Request -> ReplyIncorrect -> XML
make self a req inc = html
   [ tag "title" [Text $ "LA Feedback Service (version " ++ versionNr ++ ")"]
   ]
   [ para [ bold [Text "Term: "]
          , preString (maybe "" (prettyPrinter a) $ getTerm req) 
          ]
   , para [ bold [Text "Expected: "]
          , preString (maybe "" (prettyPrinter a) $ fromOMOBJ $ repInc_Expected inc) 
          ]
   , para [ Text "Submit the", href (reqToURL reqOk) [Text "expected"], Text "answer and continue" ]
   , hr
   , para [ bold [Text "Strategy: "], Text (req_Strategy req), br
          , bold [Text "Steps remaining: "], Text (show (repInc_Steps inc) ++ " (and " ++ show n ++ " after submitting the expected answer)"), br
          , bold [Text "Arguments: "], Text (formatArgs $ repInc_Arguments inc), br
          , bold [Text "Location: "], Text (show $ req_Location req) 
          ]
   , para [ preString $ unlines $ catMaybes $ map (showLoc (req_Location req)) $ strategyLocations $ strategy a 
          ]
   , para [ href (reqToURL reqZoomIn)  [Text "Zoom in"],  Text "to a substrategy or" 
          , href (reqToURL reqZoomOut) [Text "zoom out"], Text "to the parent strategy" 
          ]
   , hr
   , para [ bold [Text "Context:"], Text (fromMaybe "" $ req_Context req) ]
   , Text "Remove all", href (reqToURL reqNoCtxt) [Text "context information"]
   , para [ bold [Text "Derivation (so far):"], Text derivation ]
   , let f (x, y) = [ Text x, preString $ maybe "" (prettyPrinter a) (fromOMOBJ y) ]
         len = length (repInc_Derivation inc)
     in para [ bold [Text $ "Derivation (expected, " ++ show len ++ " steps):"]
             , list $ map f $ repInc_Derivation inc 
             ]
--    , preString $ concat $ map show $ repInc_Derivation inc
   ]
 where
   (reqOk, n) = expected a req inc
   reqZoomIn  = zoomIn req inc
   reqZoomOut = zoomOut req
   reqNoCtxt  = removeContext req
   reqToURL   = (self++) . oneliner . ppRequest
   formatArgs = concat . intersperse ", " . map (\(a, b) -> a ++ " = " ++ b)
   derivation = case prefixToSteps (getPrefix req (strategy a)) of
                   steps -> concat $ intersperse "; " [ name r | Step _ r <- steps, isMajorRule r ]

----------------------------------------------------------------------------
-- Actions

expected :: IsOMOBJ a => Exercise a -> Request -> ReplyIncorrect -> (Request, Int)
expected a r inc = 
   case laServerFor a r {req_Answer = Just $ repInc_Expected inc} of
      Ok ok -> ( r { req_Location = repOk_Location ok
                   , req_Term     = repInc_Expected inc
                   , req_Context  = Just (repOk_Context ok)
                   }, repOk_Steps ok )
      _ -> (r, 0)

zoomIn :: Request -> ReplyIncorrect -> Request
zoomIn r inc = r { req_Location = repInc_Location inc }

zoomOut :: Request -> Request
zoomOut r = r { req_Location = case req_Location r of
                                  [] -> []
                                  xs -> init xs }

removeContext :: Request -> Request
removeContext r = r { req_Context = Nothing }

----------------------------------------------------------------------------

showLoc :: StrategyLocation -> (StrategyLocation, StrategyOrRule a) -> Maybe String
showLoc here (loc, eitherValue) 
   | loc `isPrefixOf` here = 
        Just $ replicate (length loc*2) '.' ++ "<b>" ++ txt ++ "</b>"
   | not (null loc) && init loc `isPrefixOf` here && init loc /= here =
        Just $ replicate (length loc*2) '.' ++ txt
   | otherwise = Nothing
 where
   txt = either strategyName ((++" (rule)") . name) eitherValue
 
-- imgOUNL :: XML
-- imgOUNL = Tag "img" [("border","0"),("src","ounl.jpg"),("align","right"),("alt","OUNL")] []
 
tag :: String -> [XML] -> XML
tag s = Tag s []

br, hr :: XML
br = tag "br" []
hr = tag "hr" []

bold, para :: [XML] -> XML
bold = tag "b"
para = tag "p"

tt :: [XML] -> XML
tt = tag "tt"

preString :: String -> XML
preString = tt . f . intersperse br . map Text . lines
 where f xml = [br] ++ xml ++ [br]

list :: [[XML]] -> XML
list = tag "ul" . map (tag "li")

href :: String -> [XML] -> XML
href s = Tag "a" [("href", s)] 

html :: [XML] -> [XML] -> XML
html xs ys = tag "html" 
   [ tag "head" xs
   , tag "body" ys
   ]