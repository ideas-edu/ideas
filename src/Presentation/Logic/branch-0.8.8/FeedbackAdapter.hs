{--------------------------------------------------- 
	Copyright (c)        2006 
	Sylvia Stuurman
---------------------------------------------------}

module FeedbackAdapter where
import qualified LogicGenerator as LD
import qualified LogicFeedback as LD
import qualified EquationsFeedback as ME
import qualified EquationsGenerator as ME
import qualified Data.ByteString as B

geefOefening:: String -> String -> String -> String -> String -> String
geefOefening "proplogic" "todnf" "nl" seed params = LD.generateFormula (read seed)
geefOefening "math" "equations" "nl" seed params = ME.generateEqs (read params) 10 (read seed)
 
geefHint:: String -> String -> String -> String -> String
geefHint "proplogic" "todnf" "nl" formule = LD.hint (stripBallast (show formule))
geefHint "math" "equations" "nl" formule  = ME.hint (stripBallast (show formule))

geefNext:: String -> String -> String -> String -> String
geefNext "proplogic" "todnf" "nl" formule = LD.nextstep (stripBallast (show formule))

geefKlaar:: String -> String -> String -> String -> String -> String
geefKlaar "proplogic" "todnf" "nl" a b = geeflk (LD.hasSolved (stripBallast (show a)) (stripBallast (show b)))

geefFeedback:: String -> String -> String -> String -> String -> String
geefFeedback "proplogic" "todnf" "nl" a b = geeffb (LD.feedback (stripBallast (show a)) (stripBallast (show b)))
geefFeedback "math" "equations" "nl" a b = geeffb (ME.feedback (stripBallast (show a)) (stripBallast (show b)))

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
