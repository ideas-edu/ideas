import OpenMath.LAServer hiding (main, respond)
import Network.CGI

main = runCGI cgiMain

cgiMain :: CGI CGIResult
cgiMain = do m <- getInput "matrix"                -- read matrix xml string 
             setHeader "Content-type" "text/plain" -- return plain text
	     case m of
	     	  Just xml -> output (respond xml)
		  Nothing  -> output ("Invalid request.")

respond = fromReply . laServer . toRequest
