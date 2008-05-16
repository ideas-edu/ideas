/**
 * Deze functie wordt aangeroepen zodra write_exas (in exas/common/javascript/utils.js) de benodigde html 
* in de pagina heeft gezet.
*/
window.onload = function() {
	// $ is een functie van prototype.
	// Je krijgt het object met de id van het argument terug.
	// event handlers
 	$('aboutButton').onclick = openhelp;
	$('helpButton').onclick = openhelp;
	$('rulesButton').onclick = openhelp; 
	$('generateButton').onclick = generate;
	$('hintbutton').onclick = getHint;
	$('nextbutton').onclick = getNext;
	$('submitbutton').onclick = getFeedback;
	$('closehelpButton').onclick = closehelp;
	$('closeaboutButton').onclick = closehelp;
	$('closerulesButton').onclick = closehelp; 
}
