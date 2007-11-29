// helpfunctie om een XMLHTTP GET request te sturen
//function sendGetRequest(url, functie)
//{
	//var xmlHttp=GetXmlHttpObject()
	//if (xmlHttp==null)
	//{
		//alert ("Helaas, de browser is niet geschikt voor het on-line feedback programma")
		//return
	//} 
	//xmlHttp.open("GET", url,true);
 	//xmlHttp.onreadystatechange= new Function { functie(xmlHttp)};
 	//xmlHttp.send(null)
//}

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
