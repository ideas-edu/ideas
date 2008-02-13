/**
 * De locatie waar de services in de juiste taal staan
  */
function locatie() {
	//return "http://ideas/php/proplogic/todnf/";
	return "http://ideas.cs.uu.nl/exas/proplogic/naarDNF/nl/";
}
/**
 * Deze functie wordt aangeroepen zodra write_exas (in exas/common/javascript/utils.js) de benodigde html 
* in de pagina heeft gezet.
*/
function initialiseer()
{
	// get is een functie: document.getElementById(id);
	// event handlers
	get('aboutButton').onclick = menuhelp;
	get('helpButton').onclick = menuhelp;
	get('regelsButton').onclick = menuhelp;
	get('genereerButton').onclick = genereerFormules;
	get('sluitmenuregelsButton').onclick = sluitmenuhelp;
	get('sluitmenuhelpButton').onclick = sluitmenuhelp;
	get('sluitmenuaboutButton').onclick = sluitmenuhelp;
	get('sluitmenuregelsButton').onclick = sluitmenuhelp;
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
	var xmlHttp=createXMLHttpRequest()
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
    	
  	
   			// gegevens naar de UI-velden sturen; 
			geefWaarden(gegevens);
			get('opgave').innerHTML = presenteer(expressie);
			
			// cursor naar het werkveld
			get("werk").focus();
    	
    		// voor het onthouden van alle feedback
    		var feedbackRij = new Array();
    		feedbackRij[0] = gegevens.feedback;
    	}
}
/**	
 * nweWaarden is een object met velden 
  * opgave, uitwerking en feedback.
  */ 
function geefWaarden(nweWaarden)
{
	// De gegevens staan al in het gewenste formaat
	get("werk").value = nweWaarden.werk;
	get("feedback").innerHTML = nweWaarden.feedback;
	get("afleiding").innerHTML = nweWaarden.afleiding;
	huidige = nweWaarden.werk;
}
// expressie in ascii omzetten naar unicode voor in div
	function presenteer(opgave) {
	        var resultaatstring = opgave.replace(/~/, '&#172;&nbsp;');
	        while (resultaatstring != resultaatstring.replace(/~/, '&#172;&nbsp;')) {
	                resultaatstring = resultaatstring.replace(/~/, '&#172;&nbsp;');
	        }
	        while (resultaatstring != resultaatstring.replace('<->', '&#8596;')) {
	                resultaatstring = resultaatstring.replace('<->', '&#8596;');
	        }
	        while (resultaatstring != resultaatstring.replace('->', '&#8594;')) {
	                resultaatstring = resultaatstring.replace('->', '&#8594;');
	        }
	        while (resultaatstring != resultaatstring.replace('||', '&#8744;')) {
	                resultaatstring = resultaatstring.replace('||', '&#8744;');
	        }
	        while (resultaatstring != resultaatstring.replace('/\\', '&#8743;')) {
	                resultaatstring = resultaatstring.replace('/\\', '&#8743;');
	         }      
	        return resultaatstring;
	}
	// expressie in ascii omzetten naar unicode voor in textarea
	function presenteertekst(opgave) {
	        var resultaatstring = opgave.replace('~', String.fromCharCode(172));
	        while (resultaatstring != resultaatstring.replace(/~/, String.fromCharCode(172))) {
	                resultaatstring = resultaatstring.replace(/~/, String.fromCharCode(172));
	        }
	        while (resultaatstring != resultaatstring.replace('<->', String.fromCharCode(8596))) {
	                resultaatstring = resultaatstring.replace('<->', String.fromCharCode(8596));
	        }
	        while (resultaatstring != resultaatstring.replace('->', String.fromCharCode(8594))) {
	                resultaatstring = resultaatstring.replace('->', String.fromCharCode(8594));
	        }
	        while (resultaatstring != resultaatstring.replace('||', String.fromCharCode(8744))) {
	                resultaatstring = resultaatstring.replace('||', String.fromCharCode(8744));
	        }
	        while (resultaatstring != resultaatstring.replace('/\\', String.fromCharCode(8743))) {
	                resultaatstring = resultaatstring.replace('/\\', String.fromCharCode(8743));
	         }
	        return resultaatstring;
	}