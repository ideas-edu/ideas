}
/**
* Create and read cookies
*/
function createCookie(name,value,days) {
	if (days) {
		var date = new Date();
		date.setTime(date.getTime()+(days*24*60*60*1000));
		var expires = "; expires="+date.toGMTString();
	}
	else var expires = "";
	document.cookie = name+"="+value+expires+"; path=/";
}

function readCookie(name) {
	var nameEQ = name + "=";
	var ca = document.cookie.split(';');
	for(var i=0;i < ca.length;i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1,c.length);
		if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
	}
	return null;
}
/*
 * Javascript code which is specific for a domain and a kind of exercise.
 * Here, services can be combined.
 */
var exercisekind = "Proposition to DNF";
var id = 421;
/**
  * Global variable: usecookie
  * for OU students, will be set using the ou/index.php page
  */
var usecookie = true;
/* 
check cookie
geen cookie: laat een input zien voor het studentnummer
wel een cookie:  lezen en doorgaan
 */
document.observe("dom:loaded", checkCookie);
function checkCookie() {
	alert("We gaan cookies gebruiken");
}
