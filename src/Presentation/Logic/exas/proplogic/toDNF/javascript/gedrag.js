var teller = 0;
var antwoordVeranderd = false;
var huidige = "";
var taal = "nl";

/**
 * Deze functie wordt aangeroepen zodra write_exas (in exas/common/javascript/utils.js) de benodigde html 
* in de pagina heeft gezet.
*/
function initialiseer(dezetaal)
{
	// $ is een functie: document.getElementById(id);
	// event handlers
	addEventSimple($("werk"), 'keypress', controleer);
	addEventSimple($("werk"), 'onfocus', getCursor);
	addEventSimple($("werk"), 'onclick', getCursor);
	addEventSimple($("werk"), 'onkeyup', getCursor);
	
	// editbaar aanzetten, cursor naar het werkveld
	$("werk").readOnly=false;
	$("werk").focus();
	
	// om tekst selecteerbaar te maken in IE
	$("feedback").contentEditable = true;
	
	// de history in werking stellen
	dhtmlHistory.initialize();
	
	// event handler instellen voor wanneer er back of forward wordt aangeroepen
  	dhtmlHistory.addListener(update);
  	
  	// de gegevens bij de eerste keer
	initialLocation = "start";
	
	// Meteen een opgave genereren
	genereerFormules();
	
	// de taal vastleggen
	taal = dezetaal;
}

/** 
 * update checkt of er gegevens zijn
 * en roept geefWaarden aan. 
  */
function update(newLocation, historyData) 
{
	if (historyData != null)
	{
		geefWaarden(historyData);
	}
}

/**	
 * nweWaarden is een object met velden 
  * opgave, uitwerking en feedback.
  */ 
function geefWaarden(nweWaarden)
{
	// De gegevens staan al in het gewenste formaat
	$("werk").value = nweWaarden.werk;
	$("feedback").innerHTML = nweWaarden.feedback;
	$("afleiding").innerHTML = nweWaarden.afleiding;
	huidige = nweWaarden.werk;
}

/**
 *  genereerFormules wordt aangeroepen als de gebruiker op "Nieuwe opgave" klikt.
  * De formules worden dan in het opgaveveld en in het afleidingveld geplaatst.
  * Er wordt een xmlhttprequest gestuurd naar genereer.php. 
  */
function genereerFormules()
{
	sluitmenuhelp('help');
	sluitmenuhelp('regels');
	var url = locatie() +"genereer.php";
	var xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
	xmlHttp.open("GET", url, true);
 	xmlHttp.onreadystatechange = function() { handleFormules(xmlHttp); }; 
 	xmlHttp.send(null);
} 

/** handleFormules is de handler die wordt aangeroepen wanneer er 
  * antwoord is van de request om formules te genereren
  */
function handleFormules(xmlHttp)
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  		{
   			expressie = xmlHttp.responseText;
   			// nieuw object met nieuwe gegevens vullen
			// we zetten de gegevens meteen in het gewenste formaat
			var gegevens = new Object();
			gegevens.werk = presenteertekst(expressie);
			gegevens.afleiding = presenteer(expressie);
			gegevens.feedback = '<h4>DNF</h4><p>Het doel van de opdracht is een expressie in DNF.</p>'+
				'<h4>Werkveld</h4>'+
				'<p>Wijzig de expressie in het werkveld.<br>(Schrijf er dus geen nieuwe expressie onder!)'+
				'<h4>E&eacute;n regel</h4>'+ 
				'<p>Het is de bedoeling dat je in stappen van &eacute;&eacute;n herschrijfregel, op &eacute;&eacute;n plek, toewerkt naar een expressie die in DNF staat.</p>'+
				'<h4>Submit</h4>'+
				'<p>Geef dan aan dat je je antwoord wilt laten controleren door op Submit of op de Enter toets te drukken.<br>Voor uitgebreidere Help, klik Help in het menu aan.</p>';
    	
    		// gegevens aan de history toevoegen
   			dhtmlHistory.add(initialLocation, gegevens);
   	
   			// gegevens naar de UI-velden sturen; 
			geefWaarden(gegevens);
			$('opgave').innerHTML = presenteer(expressie);
			
			// cursor naar het werkveld
			$("werk").focus();
    	
    		// voor het onthouden van alle feedback
    		var feedbackRij = new Array();
    		feedbackRij[0] = gegevens.feedback;
    		historyStorage.put("hfeedback", feedbackRij);
    		// Als vorige antwoord nemen we de gegenereerde formules
    		historyStorage.put("vorigeAntwoord", gegevens.werk);
    		// op dezelfde manier kunnen we eventueel ook de hele afleiding bewaren
    	}
}

/** submitAntwoord wordt aangeroepen als de gebruiker de submitknop indrukt. 
  * Er wordt een xmlhttprequest gestuurd naar getfeedback.php. 
  */
function submitAntwoord()
{
	var url=locatie() +"getfeedback.php";
	var oplossing = werkveldNaarExas($("werk").value);
	var vorigeantwoord = werkveldNaarExas(historyStorage.get("vorigeAntwoord"));
	var text = "&antwoord=" + URLEncode(oplossing) + "&vorigeantwoord=" + URLEncode(vorigeantwoord) + URLEncode('\n');
	var xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
    xmlHttp.onreadystatechange = function() { feedbackOntvangen(xmlHttp);};
    xmlHttp.open('POST', url, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", text.length);
    xmlHttp.send(text);
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
function feedbackOntvangen(xmlHttp) 
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
			$("feedback").innerHTML = presenteer(URLDecode(antwoord.feedbackstring));
			// eventuele returns uit het werkveld halen
			var oplossing = schoon($("werk").value);
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
			gegevens.werk = presenteertekst(schoon($("werk").value));
			gegevens.feedback = goedgedaan();
			// het afleidingvenster wordt uitgebreid met het laatstgegeven antwoord
			gegevens.afleiding = $("afleiding").innerHTML + "<br>\n" + gegevens.werk;
			// nieuwe toestand voor de history 
    		teller = teller + 1;
	    	dhtmlHistory.add(("feedback" + teller) , gegevens);  
	    	// de huidige stap wordt gelijk aan wat in het werkveld staat.
			huidige = gegevens.werk;
			help("klaarbutton");
			
			// naar de UI velden
			geefWaarden(gegevens);
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
	var url=locatie() +"gethint.php"
	var oplossing = werkveldNaarExas(huidige);
	var text = "&formule=" + URLEncode(oplossing);
	var xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
    xmlHttp.onreadystatechange = function() { hintOntvangen(xmlHttp);};
    xmlHttp.open('POST', url, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", text.length);
    xmlHttp.send(text);
} 


function hintOntvangen(xmlHttp)
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		hint = xmlHttp.responseText;
   		// feedbackveld met de gegeven hint vullen
   		// we nemen dit niet op in de history
		$("feedback").innerHTML = hint;
    }
}

/** klaar wordt aangeroepen als de gebruiker op de klaar knop drukt 
  * submit.
  * Er wordt een xmlhttprequest gestuurd naar getklaar.php. 
  * De Klaar button verdwijnt weer 
  */
function getKlaar()
{
	var url=locatie() +"getklaar.php";
	var oplossing = werkveldNaarExas($("werk").value);
	var vorigeantwoord = werkveldNaarExas(historyStorage.get("vorigeAntwoord"));
	var text = "&antwoord=" + URLEncode(oplossing) + "&vorigeantwoord=" + URLEncode(vorigeantwoord) + URLEncode('\n');
	xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
    xmlHttp.onreadystatechange = function() { klaarOntvangen(xmlHttp); };
    xmlHttp.open('POST', url, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", text.length);
    xmlHttp.send(text);

	sluithelp("klaarbutton");
} 


function klaarOntvangen(xmlHttp)
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		klaar = xmlHttp.responseText;
   		// feedbackveld vullen met de mededeling of de student de opgave al dan niet heeft opgelost
   		// we nemen dit niet op in de history
		$("feedback").innerHTML = klaar;
		
    }
}

/** next wordt aangeroepen als de gebruiker op de next knop drukt 
  * submit.
  * Er wordt een xmlhttprequest gestuurd naar getnext.php. */
function getNext()
{
	var url=locatie() +"getnext.php"
	var oplossing = werkveldNaarExas(huidige);
	var text = "&formule=" + URLEncode(oplossing);
	xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
    xmlHttp.onreadystatechange = function() { nextOntvangen(xmlHttp); };
    xmlHttp.open('POST', url, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", text.length);
    xmlHttp.send(text);
} 


function nextOntvangen(xmlHttp)
{
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		next = xmlHttp.responseText;
   		// feedbackveld met nextstep vullen
   		// we nemen dit niet op in de history
		$("feedback").innerHTML = presenteer(next);
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