/**
 * helpfunctie om aan een element van de DOM te komen
 */
function $(id) { 
	return document.getElementById(id); 
}


/**
 * Om event handlers aan objecten te kunnen hangen
 */
function addEvent(object, event, fn) {
	if (object.addEventListener)
		object.addEventListener(event,fn,false);
	else if (object.attachEvent)
		object.attachEvent('on'+event,fn);
}

function removeEvent(object, event, fn) {
	if (object.removeEventListener)
		object.removeEventListener(event,fn,false);
	else if (object.detachEvent)
		object.detachEvent('on'+event,fn);
}

/**
 * Om de default actie van een event te voorkomen
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
// helpfunctie om een XMLHTTP POST request te sturen
function sendPostRequest(url, data, functie)
{
	var xmlHttp=GetXmlHttpObject()
	if (xmlHttp==null)
	{
		alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		return
	} 
    xmlHttp.onreadystatechange=functie 
    xmlHttp.open('POST', url, true);
    xmlHttp.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    xmlHttp.setRequestHeader("Content-length", data.length);
    xmlHttp.send(data);
}

// helpfunctie om een XMLHTTP object te krijgen
function GetXmlHttpObject()
{ 
	var objXMLHttp=null
	if (window.XMLHttpRequest) 
	{ // Mozilla, Safari,...
         objXMLHttp = new XMLHttpRequest();
         if (objXMLHttp.overrideMimeType) 
         {
            objXMLHttp.overrideMimeType('text/html');
         }
	} 
    else 
    {
		if (window.ActiveXObject) 
		{ // IE
			try 
         	{
            	objXMLHttp = new ActiveXObject("Msxml2.XMLHTTP");
        	}	catch (e) 
        	{
            		try 
            		{
              	 		objXMLHttp = new ActiveXObject("Microsoft.XMLHTTP");
               		} catch (e) {}
         	}
        }
    }
	return objXMLHttp
}


// helpfunctie om tekst te encoderen
function URLEncode(plaintext)
{
	// The Javascript escape and unescape functions do not correspond
	// with what browsers actually do...
	var SAFECHARS = "0123456789" +					// Numeric
					"ABCDEFGHIJKLMNOPQRSTUVWXYZ" +	// Alphabetic
					"abcdefghijklmnopqrstuvwxyz" +
					"-_.!~*'()";					// RFC2396 Mark characters
	var HEX = "0123456789ABCDEF";

	var encoded = "";
	for (var i = 0; i < plaintext.length; i++ ) {
		var ch = plaintext.charAt(i);
	    if (ch == " ") {
		    encoded += "+";				// x-www-urlencoded, rather than %20
		} else if (SAFECHARS.indexOf(ch) != -1) {
		    encoded += ch;
		} else {
		    var charCode = ch.charCodeAt(0);
			if (charCode > 255) {
			    alert( "Unicode Character '" 
                        + ch 
                        + "' cannot be encoded using standard URL encoding.\n" +
				          "(URL encoding only supports 8-bit characters.)\n" +
						  "A space (+) will be substituted." );
				encoded += "+";
			} else {
				encoded += "%";
				encoded += HEX.charAt((charCode >> 4) & 0xF);
				encoded += HEX.charAt(charCode & 0xF);
			}
		}
	} // for

	return encoded;
}

// helpfunctie om tekst te decoderen
function URLDecode(encoded)
{
   // Replace + with ' '
   // Replace %xx with equivalent character
   // Put [ERROR] in output if %xx is invalid.
   var HEXCHARS = "0123456789ABCDEFabcdef"; 

   var plaintext = "";
   var i = 0;
   while (i < encoded.length) {
       var ch = encoded.charAt(i);
	   if (ch == "+") {
	       plaintext += " ";
		   i++;
	   } else if (ch == "%") {
			if (i < (encoded.length-2) 
					&& HEXCHARS.indexOf(encoded.charAt(i+1)) != -1 
					&& HEXCHARS.indexOf(encoded.charAt(i+2)) != -1 ) {
				plaintext += unescape( encoded.substr(i,3) );
				i += 3;
			} else {
				alert( 'Bad escape combination near ...' + encoded.substr(i) );
				plaintext += "%[ERROR]";
				i++;
			}
		} else {
		   plaintext += ch;
		   i++;
		}
	} // while
   return plaintext;
}
