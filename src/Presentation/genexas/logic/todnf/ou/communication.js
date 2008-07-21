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

function checkCookie() {
	id = readCookie('studentnummer');
	if (id != null) {
		id = readCookie('studentnummer');
		$('number').value = id;;
		showDisplayNumber();
	}
	else {
		showAskNumber();
	}
}
function showAskNumber() {
	$('numberinput').show();
	$('numberdisplay').hide();
	if (id != 421) {
		$('number').value = id;
	}
	$('number').focus();
	$('numberbutton').observe('click', showDisplayNumber);	
	$('number').observe('keyup', checkKey);
	$('changenumberbutton').stopObserving('click', showAskNumber);
}
function showDisplayNumber() {
	id = $('number').value;
	createCookie('studentnummer', id, 90);
	$('studentnumber').update("Student number: " + id);
	$('numberinput').hide();
	$('numberdisplay').show();
	$('changenumberbutton').observe('click', showAskNumber);
	$('number').stopObserving('keyup', checkKey);
	$('numberbutton').stopObserving('click', showDisplayNumber);	
}
function checkKey(event) {	// eerst  de character code ophalen	
	if (event.keyCode == 13) {
		var number = $('number').value;
		$('number').value = number.substring(0, number.length -1);
		showDisplayNumber();
		stop(event);
		return false;
	}
}

/**
* init
*/
document.observe("dom:loaded", checkCookie);
