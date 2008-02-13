module OpenMath.Interactive (respondHTML) where

import Common.Assignment hiding (Text, Incorrect)
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

respondHTML :: String -> String
respondHTML = either (const "") (showXML . makeHTML) . pRequest

(~=) :: String -> String -> Bool
xs ~= ys = let f = map toLower . filter (not . isSpace)
           in f xs == f ys 
           
makeHTML :: Request -> XML
makeHTML req = 
   case [ ea | Entry _ ea@(ExprAssignment a) _ <- strategyTable, req_Strategy req ~= shortTitle a ] of
      [ExprAssignment a] -> 
         let noAnswer = req {req_Answer = Nothing}
         in make a noAnswer $ laServerFor a $ noAnswer
      _ -> Text "request error: unknown strategy"

make :: IsExpr a => Assignment a -> Request -> Reply -> XML
make a req (Incorrect reply) = html
   [ tag "title" [Text $ "LA Feedback Service (version " ++ versionNr ++ ")"]
   ]
   [ para [ -- href "http://www.ou.nl/" [imgOUNL]
            bold [Text "How to continue?"]
          , Text "Submit the "
          , href (reqToURL reqOk) [Text "correct"]
          , Text " answer, or zoom in to a " 
          , href (reqToURL reqSub) [Text "sub-strategy"]
          , Text "."
          ]
   , hr
   , para [ bold [Text "Strategy: "], Text (req_Strategy req), br
          , bold [Text "Steps remaining: "], Text (show $ repInc_Steps reply), br
          , bold [Text "Location: "], Text (show $ req_Location req)
          -- , list $ map (return . Text) $ catMaybes $
          --      map (fmap strategyName . flip subStrategy (strategy a)) $ inits $ req_Location req    
          ]
   , para [ preString $ unlines $ catMaybes $ map (showLoc (req_Location req)) $ reportLocations $ strategy a 
          ]
   , hr
   , para [ bold [Text "Term: "]
          , preString (maybe "" (prettyPrinter a) $ fromExpr $ req_Term req) 
          ]
   , para [ bold [Text "Expected: "]
          , preString (maybe "" (prettyPrinter a) $ fromExpr $ repInc_Expected reply) 
          ]
   ]
 where
   reqOk  = case laServerFor a req {req_Answer = Just $ repInc_Expected reply} of
               Ok r -> req {req_Location = repOk_Location r, req_Term = repInc_Expected reply }
               _    -> error "internal error: Ok expected"
   reqSub = req {req_Location = repInc_Location reply}
   reqToURL = oneliner . ppRequest
   
make _ _ _ = Text "request error: invalid request"

showLoc :: StrategyLocation -> (StrategyLocation, String) -> Maybe String
showLoc here (loc, s) 
   | loc `isPrefixOf` here = 
        Just $ replicate (length loc*2) '.' ++ "<b>" ++ s ++ "</b>"
   | not (null loc) && init loc `isPrefixOf` here && init loc /= here =
        Just $ replicate (length loc*2) '.' ++ s
   | otherwise = Nothing

oneliner :: String -> String
oneliner = (url++) . unwords . concatMap words . lines
 where url = "http://ideas.cs.uu.nl/cgi-bin/lasi.cgi?mode=html&input="
 
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