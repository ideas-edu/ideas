// taalspecifieke operators definieren:
var myLanguage = new Language();
myLanguage.addOperator(new Operator("<->", 8596, 61, 5, 2));
myLanguage.addOperator(new Operator("/\\", 8743, 97, 2, 2));
myLanguage.addOperator(new Operator("||", 8744, 111, 3, 2));
myLanguage.addOperator(new Operator("->", 8594,  105, 4, 2));
myLanguage.addOperator(new Operator("~", 172, 45, 4, 1, "Prefix"));


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
		var character = myLanguage.charcodeToUnicode(code);
		if (character != null) {
			voegin(character, "werk");
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

// expressie  omzetten naar HTML special characters voor in div
function presenteer(opgave) {
	return myLanguage.toHtml(opgave);
}

// expressie omzetten naar unicode voor in textarea
function presenteertekst(opgave) {
	var rowOfTokens = myLanguage.lex(String(opgave));
	return myLanguage.toUnicode(rowOfTokens);
}

// hulpfunctie: domeinspecifieke karakters vervangen, en enters en tabs verwijderen: vanuit textarea naar exas
function werkveldNaarExas(expressie) {
	return myLanguage.unLex(expressie);
}