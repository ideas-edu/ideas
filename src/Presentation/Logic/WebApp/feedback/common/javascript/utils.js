/**
 * deze functie wordt aangereoepen zodra de inhoud van de request die startexas heeft gedaan, binnen is.
 */
function write_exas(xmlHttp, taal) {
	if (xmlHttp.readyState==4 || xmlHttp.readyState=="complete") 
  	{
   		var antwoord = xmlHttp.responseText;
		var omgeving = $('exasdiv');
		omgeving.innerHTML = antwoord;
		initialiseer(taal);
	}
}

/**
 * helpfunctie om aan een element van de DOM te komen
 */
function $(id) { 
	return document.getElementById(id); 
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
 * Om de default actie van een event te voorkomen
 */
function stopDefaultAction(event) { 
	event.returnValue = false; 
	if (typeof event.preventDefault != "undefined") { 
		event.preventDefault(); 
	} 
}

/**
  * Om de textarea naar behoren te laten functioneren
  * is het nodig om de plaats van de cursor op te vragen 
 */
function getCursor(id) {
	if (document.selection) {
		$(id).range = document.selection.createRange();
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
 * Een tekst invoegen in een textarea
 */
function voegin(tekst, id) {
	var tekstveld = $(id);
	tekstveld.focus();
	var selectie;
	//IE en Opera
	if (document.selection) {
		selectie  = document.selection.createRange();
		prevRange = selectie.duplicate();
		prevRange.moveStart("character", -1);
		insertAtCursor(selectie, tekst);
		return;
	}
//MOZILLA/NETSCAPE  
	if (tekstveld.selectionStart) {
		var start = tekstveld.selectionStart; 
		var end   = tekstveld.selectionEnd; 
		tekstveld.value = tekstveld.value.substr(0, start) 
		+ tekst 
		+ tekstveld.value.substr(end, tekstveld.value.length); 
		tekstveld.selectionStart = start + 1;
		tekstveld.selectionEnd = start + tekst.length;
		return;		
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