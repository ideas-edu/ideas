
module Main where
import HAppS hiding(simpleHTTP)
import HAppS.Protocols.SimpleHTTP
import Control.Monad.State
import FeedbackAdapter


data MyState = MyState String deriving (Read,Show)

instance StartState MyState where
	startStateM = return $ MyState ""
instance Serialize MyState where
	typeString _ = "MyState"
	encodeStringM = defaultEncodeStringM
	decodeStringM = defaultDecodeStringM

app :: Method -> Host -> [String] -> Ev MyState Request Result

app POST _ ["exas", domein, oefening, taal, "generator"] = do
	rq <- getEvent
	put $ MyState (lookS 200 rq "seed")
	sresult 200 (geefOefening domein oefening taal (lookS 200 rq "seed") (lookS 200 rq "params"))	
	
app POST _ ["exas", domein, oefening, taal, "feedback"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "antwoord")))
	sresult 200 (geefFeedback domein oefening taal (lookS 200 rq "antwoord") (lookS 200 rq "vorigeantwoord"))
	
app POST _ ["exas", domein, oefening, taal, "hint"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "formule")))
	sresult 200 (geefHint domein oefening taal (lookS 200 rq "formule")) 
	
app POST _ ["exas", domein, oefening, taal, "next"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "formule")))
	sresult 200 (geefNext domein oefening taal (lookS 200 rq "formule")) 
	
app POST _ ["exas", domein, oefening, taal, "klaar"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "antwoord")))
	sresult 200 (geefKlaar domein oefening taal (lookS 200 rq "antwoord") (lookS 200 rq "vorigeantwoord")) 	


app _ _ _ = do
	MyState message <- get
	sresult 200 "Dit doet het hoor...."
	
main = stdMain $ simpleHTTP "" [
	("/exas/nl/proplogic/javascript/", "exas-nl-proplogic-scripts"), 
	("/exas/css/", "exas-stijlen"), 
	("/exas/nl/proplogic/html/", "exas-nl-proplogic-structuur"),
	("/dummy/", "dummy")
	] app :*: End