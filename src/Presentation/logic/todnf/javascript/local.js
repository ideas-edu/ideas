/**
 * local.js bevat alle code die taal- en plaats-afhankelijk is
 */
 
/**
 * Taalafhankelijke teksten
 * Deze is voor wanneer de stap juist was.
 */
function goedgedaan() {
		return "<p>Prima!<br> Als je denkt nu een dnf bereikt te hebben klik dan op klaar, voer anders de volgende herschrijfstap uit.</p>";
}

/**
 * De locatie waar de services in de juiste taal staan
  */
function locatie() {
	return "http://ideas/exas/proplogic/toDNF/nl/";
	// return "http://ideas.cs.uu.nl/exas/proplogic/naarDNF/nl/";
}

/**
 * Elke exas-applicatie bevat een div met id "exasdiv".
 * De applicatie wordt gesrtart door deze functie aan te roepen, met de url waar de inhoud die in de aexasdiv terecht moet komen te vinden is.
 * De aanroep moet gebeuren nadat de pagina is geladen.
 */
function startexas() {
	var url = locatie() + "html/structuur.php";
	var xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
	xmlHttp.open("GET", url, true);
 	xmlHttp.onreadystatechange = function() { write_exas(xmlHttp, "nl"); }; 
 	xmlHttp.send(null)
	//sendGetRequest(url, write_exas);
}
