// wordt aangeroepen wanneer de gebruiker in het werkveld typt
function controleer(e) {	// eerst  de character code ophalen	
	e = getEvent(e);
	var code = getCode(e);	
	if (code == 13) {
		submitAntwoord();
		stop(e);
		return false;
	}
	// We hoeven geen conversies te doen: de formules kunnen met gewone ascii tekens worden geschreven
	return true;
}

// vertaal asci karaktercode naar unicode
function vertaal(charcode) {
	return charcode;
}

// spaties tussen getallen en operatoren zetten voor in de div
function presenteer(opgave) {
	var resultstring = opgave.replace(/\*/g, "&nbsp;*&nbsp;");
	resultstring = resultstring.replace(/\+/g,  "&nbsp;+&nbsp;");
	resultstring = resultstring.replace(/\-/g,  "&nbsp;-&nbsp;");
	resultstring = resultstring.replace(/=/g,  "&nbsp;=&nbsp;");
	resultstring = resultstring.replace(/\n/g,  "<br>");
	return resultstring;
}

// expressie in ascii omzetten naar unicode voor in textarea
function presenteertekst(opgave) {
	return opgave;
}

//  expressie in unicode uit textarea omzetten naar ascii
function naarAscii(expressie) {
	 return expressie;
}

// hulpfunctie: domeinspecifieke karakters vervangen, en enters en tabs verwijderen: vanuit textarea naar exas
function werkveldNaarExas(expressie) {
	var oplossing = expressie.replace("\r", "");
	oplossing = oplossing.replace("\t", "");
	return oplossing;
}