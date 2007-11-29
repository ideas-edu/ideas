
module Main where
import HAppS
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

app GET _ ["math", "generator"] = do
	rq <- getEvent
	put $ MyState geefStandaardFormule 
	sresult 200 geefStandaardFormule 
	
app POST _ ["math", "generator"] = do
	rq <- getEvent
	put $ MyState (lookS 200 rq "seed")
	sresult 200 (geefFormule (lookS 200 rq "seed"))
	
app POST _ ["math", "feedback"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "antwoord")))
	sresult 200 (geefMathFeedback (lookS 200 rq "antwoord") (lookS 200 rq "vorigeantwoord"))
	
app POST _ ["logic", "generator"] = do
	rq <- getEvent
	put $ MyState (lookS 200 rq "seed")
	sresult 200 (geefLogica (lookS 200 rq "seed"))
	
	
app POST _ ["logic", "feedback"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "antwoord")))
{-	put $ MyState (lookS 200 rq "antwoord")-}
	sresult 200 (geefLogicFeedback (lookS 200 rq "antwoord") (lookS 200 rq "vorigeantwoord"))
	
app POST _ ["logic", "hint"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "formule")))
	sresult 200 (geefLogicaHint (lookS 200 rq "formule")) 
	
app POST _ ["logic", "next"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "formule")))
	sresult 200 (geefLogicaNext (lookS 200 rq "formule")) 
	
app POST _ ["logic", "klaar"] = do
	rq <- getEvent
	put $ MyState (stripBallast (show (lookS 200 rq "antwoord")))
	sresult 200 (geefLogicaKlaar (lookS 200 rq "antwoord") (lookS 200 rq "vorigeantwoord")) 	
	
app _ _ _ = do
	MyState message <- get
	sresult 200 message
	
main = stdMain $ simpleHTTP "" [] app :*: End
