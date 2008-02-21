module OpenMath.Interactive (respondHTML, oneliner) where

import Common.Context
import Common.Assignment hiding (Text, Incorrect)
import Common.Transformation
import Common.Strategy hiding (not)
import OpenMath.LAServer
import OpenMath.StrategyTable
import OpenMath.ObjectParser
import OpenMath.Request
import OpenMath.Reply
import OpenMath.XML
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
   case [ (ea, laServerFor a noAnswer) | Entry _ ea@(ExprAssignment a) _ _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [(ExprAssignment a, Incorrect inc)] -> make self a noAnswer inc
      [_] -> Text "request error: invalid request"
      []  -> Text "request error: unknown strategy"
      _   -> Text "request error: ambiguous strategy"
 where
   noAnswer = req {req_Answer = Nothing}
               
make :: IsExpr a => String -> Assignment (Context a) -> Request -> ReplyIncorrect -> XML
make self a req inc = html
   [ tag "title" [Text $ "LA Feedback Service (version " ++ versionNr ++ ")"]
   ]
   [ para [ bold [Text "Term: "]
          , preString (maybe "" (prettyPrinter a) $ getContextTerm req) 
          ]
   , para [ bold [Text "Expected: "]
          , preString (maybe "" (prettyPrinter a . inContext) $ fromExpr $ repInc_Expected inc) 
          ]
   , para [ Text "Submit the", href (reqToURL reqOk) [Text "expected"], Text "answer and continue" ]
   , hr
   , para [ bold [Text "Strategy: "], Text (req_Strategy req), br
          , bold [Text "Steps remaining: "], Text (show (repInc_Steps inc) ++ " (and " ++ show n ++ " after submitting the expected answer)"), br
          , bold [Text "Location: "], Text (show $ req_Location req) 
          ]
   , para [ preString $ unlines $ catMaybes $ map (showLoc (req_Location req)) $ reportLocations $ strategy a 
          ]
   , para [ href (reqToURL reqZoomIn)  [Text "Zoom in"],  Text "to a substrategy or" 
          , href (reqToURL reqZoomOut) [Text "zoom out"], Text "to the parent strategy" 
          ]
   , hr
   , para [ bold [Text "Context:"], Text (fromMaybe "" $ req_Context req) ]
   , para [ bold [Text "Derivation:"], Text derivation ]
   , Text "Remove all", href (reqToURL reqNoCtxt) [Text "context information"]
   ]
 where
   (reqOk, n) = expected a req inc
   reqZoomIn  = zoomIn req inc
   reqZoomOut = zoomOut req
   reqNoCtxt  = removeContext req
   reqToURL   = (self++) . oneliner . ppRequest
   derivation = case prefixToSteps (getPrefix req) (strategy a) of
                   Just steps -> concat $ intersperse "; " [ name r | Major r <- steps ]
                   _ -> []
               

----------------------------------------------------------------------------
-- Actions

expected :: IsExpr a => Assignment (Context a) -> Request -> ReplyIncorrect -> (Request, Int)
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

showLoc :: StrategyLocation -> (StrategyLocation, String) -> Maybe String
showLoc here (loc, s) 
   | loc `isPrefixOf` here = 
        Just $ replicate (length loc*2) '.' ++ "<b>" ++ s ++ "</b>"
   | not (null loc) && init loc `isPrefixOf` here && init loc /= here =
        Just $ replicate (length loc*2) '.' ++ s
   | otherwise = Nothing
 
imgOUNL :: XML
imgOUNL = Tag "img" [("border","0"),("src","ounl.jpg"),("align","right"),("alt","OUNL")] []
 
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