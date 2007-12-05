// wordt aangeroepen wanneer de gebruiker in het werkveld typt
function controleer(e) {	// eerst  de character code ophalen	
	e = getEvent(e);
	var code = getCode(e);	
	if (code == 13) {
		submitAntwoord();
		stop(e);
		return false;
	}
	else {
		switch (code) {
			// - voor negatie
			case 45 : 
				voegin(String.fromCharCode(172), "werk");
				stop(e);
				return false;
			// = voor equivalentie
			case 61:
				voegin(String.fromCharCode(8596), "werk");
				stop(e);
				return false;
			//  o voor or
			case 111 :
				voegin(String.fromCharCode(8744), "werk");
				stop(e);
				return false;
			// i  voor implicatie
			case 105 :
				voegin(String.fromCharCode(8594), "werk");
				stop(e);
				return false;
			// a voor and
			case 97 : 
				voegin(String.fromCharCode(8743), "werk");
				stop(e);
				return false;				
		}
	}
	//e.returnValue = true;
	return true;
}

// vertaal asci karaktercode naar unicode
function vertaal(charcode) {
	switch (charcode) {
	// - voor negatie
	case 45 : 
		return String.fromCharCode(172);
		break;
	// = voor equivalentie
	case 61:
		return String.fromCharCode(8596);
		break;
	//  o voor or
	case 111 :
		return String.fromCharCode(8744);
		break;
	// i  voor implicatie
	case 105 :
		return String.fromCharCode(8594);
		break;
	// a voor and
	case 97 : 
		return String.fromCharCode(8743);
		break;
	default :
		return String.fromCharCode(charCode);
	}
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

//  expressie in unicode uit textarea omzetten naar ascii
function naarAscii(expressie) {
	var resultaatstring = expressie.replace(String.fromCharCode(172), '~');
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(172), '~')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(172), '~');
	}
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(8596), '<->')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(8596), '<->');
	}
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(8594), '->')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(8594), '->');
	}
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(8744), '||')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(8744), '||');
	}
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(8743), '/\\')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(8743), '/\\');
	 }
	 return resultaatstring;
}

// hulpfunctie: domeinspecifieke karakters vervangen, en enters en tabs verwijderen: vanuit textarea naar exas
function werkveldNaarExas(expressie) {
	return schoon(naarAscii(expressie));
}