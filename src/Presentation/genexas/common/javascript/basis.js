
/*************************************************
 *  korte versie van document.GetElementById
  *************************************************/
function get(element) {
 return document.getElementById(element);
}

/*  **********************************************
 *   toggle functie.
 *  gebruik:
 *   addEvent(get('toggler'), 'click', function() {
 *   toggle('example');
 *   toggle('foo', 'bar', 'baz', 'thunk');  })
  **************************************************/
function toggle() {
	var element;
  for (var i=0, elementID; elementID = arguments[i]; i++) {
	element = $(elementID);
    element.style.display = (element.style.display != 'none' ? 'none' : '' );
  }
}


/****************************************************
 * iets toevoegen aan het windows.onload event
 * kan ook met:
 * addEvent(window,'load',func1,false)
 * maar met addLoadEvent kun je sneller zien wat er gebeurt 
 * na een page load
 ****************************************************/
function addLoadEvent(functie) {
	var oldonload = window.onload;
	if (typeof window.onload != 'function') {
		window.onload = functie;
	}
	else {
		window.onload = function() {
			oldonload();
			functie();
		}
	}
}
/****************************************************
 * xmlhttprequest cre?ren
 *****************************************************/
function createXMLHttpRequest() {
  try { return new ActiveXObject("Msxml2.XMLHTTP"); } catch (e) {}
  try { return new ActiveXObject("Microsoft.XMLHTTP"); } catch (e) {}
  try { return new XMLHttpRequest(); } catch(e) {}
  alert("XMLHttpRequest not supported");
  return null;
}

/****************************************************
 * xmlhttprequest gebruiken
 *****************************************************/
function stuurPostRequest(url, gegevens, callback) {
	 var xhr = createXMLHttpRequest();
	 try {
		xhr.open('POST',  url);
	    xhr.setRequestHeader('Content-Type', "text/xml");
		xhr.onreadystatechange = callback;
		xhr.send(gegevens);
	}
	catch(e){}
	finally{}
}
