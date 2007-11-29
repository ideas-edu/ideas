var xmlHttp
var teller = 0

// wordt aangeroepen wanneer de pagina is geladen
function initialiseer()
{
	// de velden
	var invoerVeld = document.getElementById("invoer")
	var feedbackVeld = document.getElementById("feedback")
	var uitwerkingVeld = document.getElementById("uitwerking")
	var voortgangVeld = document.getElementById("voortgang")
	
	// wel of niet editbaar aanzetten
	invoerVeld.readOnly=false
	feedbackVeld.readOnly=true
	uitwerkingVeld.readOnly=true
	voortgangVeld.readOnly=true
	
	// de history in werking stellen
	dhtmlHistory.initialize()
	// event handler instellen voor wanneer er back of forward wordt aangeroepen
  	dhtmlHistory.addListener(update)
  	
  	// de gegevens bij de eerste keer
	initialLocation = "start"
	genereerFormules()
}

/** update checkt of er gegevens zijn
  * en roept geefWaarden aan. */
function update(newLocation, historyData) 
{
	if (historyData != null)
	{
		geefWaarden(historyData)
	}
}

/**	nweWaarden is een object met velden 
  * formule, voortgang, antwoord en feedback.
  * $ is een helperfunctie voor document.getElementById  
  * De velden van de user interface met de overeenkomstige namen 
  * krijgen nieuwe waarden */
  
function geefWaarden(nweWaarden)
{
	$("invoer").value = nweWaarden.invoer
	$("feedback").value = nweWaarden.feedback
	$("uitwerking").value = nweWaarden.uitwerking
	$("voortgang").value = nweWaarden.voortgang
}

/** genereerFormules wordt aangeroepen na het laden van de pagina.
  * De formules worden dan in het werkveld en in de history geplaatst.
  * Er wordt een xmlhttprequest gestuurd naar genereer.php. */
  
function genereerFormules()
{
	sendGetRequest("http://localhost/feedback/math/genereer.php", handleFormules);
} 

/** handleFormules is de handler die wordt aangeroepen wanneer er 
  * antwoord is van de request om formules te genereren
  */

function handleFormules()
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  		{
   			antwoord = xmlHttp.responseText
   			// nieuw object met nieuwe gegevens vullen
			var gegevens = new Object();
			gegevens.invoer = antwoord
			gegevens.uitwerking = antwoord
  			gegevens.feedback = "Feedback..."
    		gegevens.voortgang = "Voortgang..."
    	
    		// gegevens aan de history toevoegen
   			dhtmlHistory.add(initialLocation, gegevens)
   	
   			// gegevens naar de UI-velden sturen
    		geefWaarden(gegevens)
    	
    		// voor het onthouden van alle feedback
    		var feedbackRij = new Array()
    		feedbackRij[0] = ""
    		historyStorage.put("hfeedback", feedbackRij)
    		// Als vorige antwoord nemen we de gegenereerde formules
    		historyStorage.put("vorigeAntwoord", antwoord);
    		// op dezelfde manier kunnen we ook de hele voortgang bewaren
    	}
}

/** submitAntwoord wordt aangeroepen als de gebruiker een antwoord 
  * submit.
  * Op dit moment wordt het antwoord ook in een loggingveld getoond.
  * Voor de eerste release moet dat worden weggehaald. 
  * Er wordt een xmlhttprequest gestuurd naar getfeedback.php. */
function submitAntwoord()
{
	var url="http://localhost/feedback/math/getfeedback.php"
	var text = "&antwoord=" + URLEncode(document.getElementById("invoer").value) + "&vorigeantwoord=" + URLEncode(historyStorage.get("vorigeAntwoord"));
	$("logging").value = text
	sendPostRequest(url, text, feedbackOntvangen)
} 

/** Wanneer de boodschap van de xmlhttrequest binnen is komt feedbackOntvangen 
  * in actie.
  * Er wordt een nieuwe object voor de nieuwe gegevens aangemaakt.
  * Die nieuwe gegevens worden in de history gezet.
  * En de UI-velden worden van nieuwe gegevens voorzien. 
  * Interpretatie van wat er terugkomt:
  * uitkomst=False betekent: het antwoord was een juiste herschrijving
  * de ingevoerde formule kan dus in de history worden opgenomen.
  * uitkomst=True betekent: het wantwoord was geen juiste herschrijving
  * de ingevoerde formule wordt dus niet in de history opgenomen.
  * (maar wordt wel getoond in het venster met alle ingevoerde antwoorden)*/
function feedbackOntvangen() 
{ 
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete")
	{ 
		var antwoord = eval("(" + xmlHttp.responseText + ")");
		
		// nieuw object met nieuwe gegevens vullen
		var gegevens = new Object();
		// als de uitkomst True is, is het laatstgegeven antwoord onjuist
		// de history wordt dan niet uitgebreid met het laatstgegeven antwoord
		// if ((antwoord.feedbackstring).length > 3)
		if ((antwoord.uitkomst) != "False")
		{
			gegevens.invoer = historyStorage.get("vorigeAntwoord")
		}
		// zo niet, dan gebruiken we het laatstgegeven antwoord
		// de history wordt aangevuld
		else 
		{
			gegevens.invoer = document.getElementById("invoer").value
			historyStorage.put("vorigeAntwoord", gegevens.invoer)			
		}
		gegevens.uitwerking = document.getElementById("uitwerking").value + "\n" + document.getElementById("invoer").value
    	gegevens.voortgang = antwoord.indicatorstring
    	gegevens.feedback = antwoord.feedbackstring
    	// we gebruiken alleen de laatste feedback
    	// als we alle feedback willen onthouden, kan dat met historyStorage
    	
    	// nieuwe toestand voor de history 
    	teller = teller + 1
	    dhtmlHistory.add(("feedback" + teller) , gegevens)    
	    
	    // onthouden van alle feedback
    	var feedbackRij = historyStorage.get("hfeedback")
    	feedbackRij[teller] = gegevens.feedback
    	historyStorage.put("hfeedback", feedbackRij)
    	
	    // nieuwe waarden naar de UI-velden sturen
    	geefWaarden(gegevens)
	} 
} 

/* We willen de enter-key afvangen:
 * De gebruiker kan dan z'n antwoord met een enter afsluiten.
 */
function enter() 
{
   if ( event.keyCode == "13" ) 
   {
      submitAntwoord();
   }
}