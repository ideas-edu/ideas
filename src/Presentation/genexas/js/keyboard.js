/**
 * helpfunctie om aan een element van de DOM te komen
 */
function getElemId(id) { 
	return document.getElementById(id); 
}

/**
 * Een tekst invoegen in een textarea
 */
function voegin(tekst, id) {
	var tekstveld = getElemId(id);
	tekstveld.focus();
	var selectie;
	//IE en Opera
	if (document.selection) {
		selectie  = document.selection.createRange();
		prevRange = selectie.duplicate();
		prevRange.moveStart("character", -1);
		insertAtCursor(selectie, tekst);
		return;
	} else if (tekstveld.selectionStart || tekstveld.selectionStart == '0') { //MOZILLA/NETSCAPE  
		var start = tekstveld.selectionStart; 
		var end   = tekstveld.selectionEnd; 
		tekstveld.value = tekstveld.value.substr(0, start) 
		+ tekst 
		+ tekstveld.value.substr(end, tekstveld.value.length); 
		tekstveld.selectionStart = start + 1;
		tekstveld.selectionEnd = start + tekst.length;
		return;		
	} else {
	  tekstveld.value += tekst;
	}
 }
 
/**
 * Wordt aangeroepen vanuit voegin
* doet het eigenlijke werk
*/ 
function insertAtCursor(range, tekst) {
	range.text = tekst;
	range.collapse(false);
	range.select();
}

// wordt aangeroepen wanneer de gebruiker in het werkveld typt
function controleer(e) {	// eerst  de character code ophalen	
	e = getEvent(e);
	var code = getCode(e);	
	if (code == 13) {
		getFeedback('enter key');
		stop(e);
		return false;
	}
	else {
		switch (code) {
			// - voor negatie
			case 45 : 
				voegin(String.fromCharCode(172), "work");
				stop(e);
				return false;
			// = voor equivalentie
			case 61:
				voegin(String.fromCharCode(8596), "work");
				stop(e);
				return false;
			//  o voor or
			case 111 :
				voegin(String.fromCharCode(8744), "work");
				stop(e);
				return false;
			// i  voor implicatie
			case 105 :
				voegin(String.fromCharCode(8594), "work");
				stop(e);
				return false;
			// a voor and
			case 97 : 
				voegin(String.fromCharCode(8743), "work");
				stop(e);
				return false;
			// laat p, q, r, T, F, ( en ) en spatie door
			case 112 : case 113 : case 114 : case 32 : case 84 : case 70 : case 40 : case 41 :
			     	 return true;
			// drop alle andere invoer
			default :
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
	while (resultaatstring != resultaatstring.replace(String.fromCharCode(8743), '/\\\\')) {
		resultaatstring = resultaatstring.replace(String.fromCharCode(8743), '/\\\\');
	}
        return resultaatstring;
}

// hulpfunctie: domeinspecifieke karakters vervangen, en enters en tabs verwijderen: vanuit textarea naar exas
function werkveldNaarExas(expressie) {
	return schoon(naarAscii(expressie));
}

/**
* Om event handlers aan objecten te kunnen hangen
*/
function addEventSimple(object,event,fn) {
        if (object.addEventListener)
                object.addEventListener(event,fn,false);
        else if (object.attachEvent)
                object.attachEvent('on'+event,fn);
}
						  
function removeEventSimple(object,event,fn) {
        if (object.removeEventListener)
	        object.removeEventListener(event,fn,false);
        else if (object.detachEvent)
                object.detachEvent('on'+event,fn);
}

/**
 * Om cross-browser het event-object te pakken te krijgen
 */
function getEvent(event) {
	if (typeof event == "undefined") {
		var event = window.event;	
	}
	return event;
}

/**
 * om cross-browser de keycode te pakken te krijgen
 */
function getCode(event) {
	var code;
	if (event.keyCode) {
		// Opera en IE
		code = event.keyCode;	
	}
	else {
		// Firefox
		code = event.which;	
	}
	return code;
}

/**
 * om cross-browser de default action te voorkomen
  */
function stop(event) {
	event.cancelBubble = true;
	event.returnValue = false;
	if (event.stopPropagation) {
		event.stopPropagation();
		event.preventDefault();
	}
}

/** 
 * Een functie om tabs en nieuwe regeltekens weg te halen,
  */
function schoon(tekst) {
	var oplossing = tekst.replace("\n", "");
	oplossing = oplossing.replace("\r", "");
	oplossing = oplossing.replace("\t", "");
	return oplossing;
}

/**
 * Om cross-browser aan de juiste selectie te komen
*/ 
function getRangeObject(selectionObject) {
	if (selectionObject.getRangeAt)
		return selectionObject.getRangeAt(0);
	else { // Safari!
		var range = document.createRange();
		range.setStart(selectionObject.anchorNode,selectionObject.anchorOffset);
		range.setEnd(selectionObject.focusNode,selectionObject.focusOffset);
		return range;
	}
}
