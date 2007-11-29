var xmlHttp;
var teller = 0;
var antwoordVeranderd = false;
var huidige = "";
var locatie = "http://ideas.cs.uu.nl/feedback/logic/";

/// wordt aangeroepen wanneer de pagina is geladen
function initialiseer()
{
	// $ is een functie: document.getElementById(id);
	// editbaar aanzetten, cursor naar het werkveld
	$("feedback").readOnly=true;
	$("werk").readOnly=false;
	$("werk").focus();
	
	
	// de history in werking stellen
	dhtmlHistory.initialize();
	
	// event handler instellen voor wanneer er back of forward wordt aangeroepen
  	dhtmlHistory.addListener(update);
  	
  	// de gegevens bij de eerste keer
	initialLocation = "start";
	
	// Meteen een opgave genereren
	genereerFormules();
}

function controleer(e) {
	var code;
	if (!e) {
		var e = window.event;	
	}
	if (e.keyCode) {
		code = e.keyCode;	
	}
	else {
		if (e.which) {
			code = e.which;	
		}
		else {
			code = e.charCode;
		}	
	}
	if (code == 13) {
		submitAntwoord();
		return true;
	}
	else {
		return false;
	}
}

/** update checkt of er gegevens zijn
  * en roept geefWaarden aan. 
  */
function update(newLocation, historyData) 
{
	if (historyData != null)
	{
		geefWaarden(historyData);
	}
}

/**	nweWaarden is een object met velden 
  * opgave, uitwerking en feedback.
  */ 
function geefWaarden(nweWaarden)
{
	$("werk").value = nweWaarden.werk;
	$("feedback").value = nweWaarden.feedback;
	$("afleiding").innerHTML = nweWaarden.afleiding;
	huidige = nweWaarden.werk;
}

/** genereerFormules wordt aangeroepen als de gebruiker op "Nieuwe opgave" klikt.
  * De formules worden dan in het opgaveveld en in het afleidingveld geplaatst.
  * Er wordt een xmlhttprequest gestuurd naar genereer.php. 
  */
function genereerFormules()
{
	sluitmenuhelp('help');
	sluitmenuhelp('regels');
	sendGetRequest(locatie+"genereer.php", handleFormules);
} 

/** handleFormules is de handler die wordt aangeroepen wanneer er 
  * antwoord is van de request om formules te genereren
  */
function handleFormules()
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  		{
   			antwoord = xmlHttp.responseText;
   			// nieuw object met nieuwe gegevens vullen
			var gegevens = new Object();
			gegevens.opgave = antwoord;
			gegevens.werk = antwoord;
			gegevens.afleiding = antwoord;
			gegevens.feedback = "";
    	
    		// gegevens aan de history toevoegen
   			dhtmlHistory.add(initialLocation, gegevens);
   	
   			// gegevens naar de UI-velden sturen; 
			// feedback en werkveld leegmaken.
			$('opgave').innerHTML = antwoord;
			$('werk').value = antwoord;
			$("feedback").value = "Wijzig de expressie in het werkveld.\n(Schrijf er dus geen nieuwe expressie onder!)\nHet is de bedoeling dat je in stappen toewerkt naar een expressie die in DNF staat.\nGeef dan aan dat je je antwoord wilt laten controleren door op Submit of op de Enter toets te drukken.\nVoor uitgebreidere Help, klik Help in het menu aan.";
			$("afleiding").innerHTML = antwoord;
			huidige = antwoord;
			
			// cursor naar het werkveld
			$("werk").focus();
    	
    		// voor het onthouden van alle feedback
    		var feedbackRij = new Array();
    		feedbackRij[0] = "";
    		historyStorage.put("hfeedback", feedbackRij);
    		// Als vorige antwoord nemen we de gegenereerde formules
    		historyStorage.put("vorigeAntwoord", antwoord);
    		// op dezelfde manier kunnen we eventueel ook de hele afleiding bewaren
    	}
}

/** submitAntwoord wordt aangeroepen als de gebruiker de submitknop indrukt. 
  * Er wordt een xmlhttprequest gestuurd naar getfeedback.php. 
  */
function submitAntwoord()
{
	var url=locatie+"getfeedback.php";
	var oplossing = $("werk").value;
	// enters en tabs verwijderen
	oplossing = schoon(oplossing);
	var text = "&antwoord=" + URLEncode(oplossing) + "&vorigeantwoord=" + URLEncode(historyStorage.get("vorigeAntwoord")) + URLEncode('\n');
	sendPostRequest(url, text, feedbackOntvangen);
	antwoordVeranderd = false;
} 

/** Wanneer de boodschap van de xmlhttrequest binnen is komt feedbackOntvangen in actie.
  * Er wordt een nieuwe object voor de nieuwe gegevens aangemaakt.
  * Die nieuwe gegevens worden in de history gezet.
  * De UI-velden worden van nieuwe gegevens voorzien. 
  *
  * Interpretatie van wat er terugkomt:
  * uitkomst=False betekent: het antwoord was een juiste herschrijving
  * de ingevoerde formule kan dus in de history worden opgenomen.
  * uitkomst=True betekent: het antwoord was geen juiste herschrijving
  * de ingevoerde formule wordt dus niet in de history opgenomen.
  */
function feedbackOntvangen() 
{ 
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete")
	{ 
		var antwoord = eval("(" + xmlHttp.responseText + ")");
		
		// nieuw object met nieuwe gegevens vullen
		var gegevens = new Object();
    	
		// als de uitkomst True is, is het laatstgegeven antwoord onjuist:
		// de history wordt niet uitgebreid met het laatstgegeven antwoord,
		// er is een feedbackstring,
		// het gegeven antwoord blijft staan in het werkveld, 
		// een eventueel aanwezige Klaar button verdwijnt.
		if ((antwoord.uitkomst) == "True")
		{
			// feedback naar het feedbackveld
			$("feedback").value = URLDecode(antwoord.feedbackstring);
			// eventuele returns uit het werkveld halen
			var oplossing = schoon($("werk").value);
			$("werk").value = oplossing;
			// klaarbutton uitzetten
			sluithelp("klaarbutton");
			
		}
		// zo niet, dan is het antwoord in het werkveld juist:
		// de history wordt aangevuld,
		// eventuele tekst in het feedbackveld wordt weggehaald,
		// de expressie in het opgaveveld wordt vervangen door de nieuwe expressie,
		// er verschijnt een Klaar button.
		else 
		{
			// de expressie komt in de history
			historyStorage.put("vorigeAntwoord", schoon($("werk").value));
			// vullen van het gegevens object voor de history
			gegevens.opgave = $("opgave").innerHTML;
			gegevens.werk = schoon($("werk").value);
			gegevens.feedback = "Prima, ga zo door!";
			// het afleidingvenster wordt uitgebreid met het laatstgegeven antwoord
			gegevens.afleiding = $("afleiding").innerHTML + "<br>\n" + gegevens.werk;
			// nieuwe toestand voor de history 
    		teller = teller + 1;
	    	dhtmlHistory.add(("feedback" + teller) , gegevens);  
	    	// de huidige stap wordt gelijk aan wat in het werkveld staat.
			huidige = gegevens.werk;
			// eventuele returns uit het werkveld halen
			$("werk").value = gegevens.werk;
			help("klaarbutton");
			
			// naar de UI velden
			$('feedback').value = "Prima, ga zo door!";
			$('afleiding').innerHTML = gegevens.afleiding;
		}  
	    
	    // onthouden van alle feedback
    	var feedbackRij = historyStorage.get("hfeedback");
    	feedbackRij[teller] = gegevens.feedback;
    	historyStorage.put("hfeedback", feedbackRij);
	} 
} 

/** hint wordt aangeroepen als de gebruiker op de hint knop drukt 
  * submit.
  * Er wordt een xmlhttprequest gestuurd naar gethint.php. */
function getHint()
{
	var url=locatie+"gethint.php"
	var oplossing = huidige;
	var text = "&formule=" + URLEncode(oplossing);
	sendPostRequest(url, text, hintOntvangen);
} 


function hintOntvangen()
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		hint = xmlHttp.responseText;
   		// feedbackveld met de gegeven hint vullen
   		// we nemen dit niet op in de history
		$("feedback").value = hint;
    }
}

/** klaar wordt aangeroepen als de gebruiker op de klaar knop drukt 
  * submit.
  * Er wordt een xmlhttprequest gestuurd naar getklaar.php. 
  * De Klaar button verdwijnt weer 
  */
function getKlaar()
{
	var url=locatie+"getklaar.php";
	var oplossing = schoon($("werk").value);
	var text = "&antwoord=" + URLEncode(oplossing) + "&vorigeantwoord=" + URLEncode(historyStorage.get("vorigeAntwoord")) + URLEncode('\n');
	sendPostRequest(url, text, klaarOntvangen);
	sluithelp("klaarbutton");
} 


function klaarOntvangen()
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		klaar = xmlHttp.responseText;
   		// feedbackveld vullen met de mededeling of de student de opgave al dan niet heeft opgelost
   		// we nemen dit niet op in de history
		$("feedback").value = klaar;
		
    }
}

/** next wordt aangeroepen als de gebruiker op de next knop drukt 
  * submit.
  * Er wordt een xmlhttprequest gestuurd naar getnext.php. */
function getNext()
{
	var url=locatie+"getnext.php"
	var oplossing = huidige;
	var text = "&formule=" + URLEncode(oplossing);
	sendPostRequest(url, text, nextOntvangen);
} 


function nextOntvangen()
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		next = xmlHttp.responseText;
   		// feedbackveld met nextstep vullen
   		// we nemen dit niet op in de history
		$("feedback").value = next;
    }
}

/** Deze functie vervangt de laatst toegevoegde formule door de vorige.
  * We hoeven niets aan de history te doen: die is al in orde gemaakt toen 
  * het bericht binnenkwam dat de laatst ingevoerde oplossing niet juist was.
  *
  * Let op! Dit gaat fout als de student op herstel drukt terwijl het vorige antwoord
  * juist was!
  * 
  * TODO? feedback ook weghalen?
  */
function herstel() 
{ 
		$("werk").value = historyStorage.get("vorigeAntwoord");
		huidige = historyStorage.get("vorigeAntwoord");
}

/** Een functie om tabs en nieuwe regeltekens weg te halen, en om 
  * \/ te vervangen door ||
  */
function schoon(tekst) {
	oplossing = tekst.replace("\n", "");
	oplossing = oplossing.replace("\r", "");
	oplossing = oplossing.replace("\t", "");
	oplossing = oplossing.replace("\\/", "||");
	return oplossing;
}