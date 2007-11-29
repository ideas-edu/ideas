{--------------------------------------------------- 
	Copyright (c)        2006 
	Sylvia Stuurman
---------------------------------------------------}

module FeedbackAdapter where
import LogicGenerator
import qualified LogicFeedback as L
import qualified EquationsFeedback as M
import EquationsGenerator
import qualified Data.ByteString as B
     
geefFormule:: String -> String		        
geefFormule seed = generateEqs 2 10 (read seed)

geefLogicaHint:: String -> String
geefLogicaHint formule = L.hint (stripBallast (show formule))

geefLogicaNext:: String -> String
geefLogicaNext formule = L.nextstep (stripBallast (show formule))

geefLogicaKlaar:: String -> String -> String
geefLogicaKlaar a b = geeflk (L.hasSolved (stripBallast (show a)) (stripBallast (show b)))

geefLogica:: String -> String		        
geefLogica seed = generateFormula (read seed)

geefStandaardFormule::  String		        
geefStandaardFormule  = generateEqs 2 10 5

geefMathFeedback::String -> String -> String
geefMathFeedback antwoord vorigeantwoord = geeffb (M.feedback (stripBallast (show antwoord)) (stripBallast (show vorigeantwoord)))


geefLogicFeedback::String -> String -> String
geefLogicFeedback a b = geeffb (L.feedback (stripBallast (show a)) (stripBallast (show b)))
 {----------------------------------------------- 
      Help functions
      12-2006-Sylvia Stuurman
      -----------------------------------------------}
geeffb (True,feedbackstring,indicatorstring) = "{"++"\"uitkomst\":"++"\"True\""++",\"feedbackstring\":"++(show feedbackstring)++",\"indicatorstring\":"++(show indicatorstring)++"}"
geeffb (False,feedbackstring,indicatorstring) = "{"++"\"uitkomst\":"++"\"False\""++",\"feedbackstring\":"++(show feedbackstring)++",\"indicatorstring\":"++(show indicatorstring)++"}"

geeflk (uitkomst,klaarstring,indicatorstring) = klaarstring

stripBallast :: String -> String
stripBallast ('"':string) = stripBallast string
   -- remove double quotes
stripBallast ('\\':'n':string) = '\n':stripBallast string
   -- replace \\n by \n
stripBallast ('\\':'\\':'\\':'\\':string) = '\\':stripBallast string
   -- replace \\\\ by \\
stripBallast ('\\':'\\':string) = '\\':stripBallast string
   -- replace \\\\ by \\
stripBallast (a:string) = (a:(stripBallast string))
stripBallast [] = []

naarString :: B.ByteString -> String
naarString bytestring = show  bytestring
