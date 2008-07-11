/* *
  * This file contains the initialisation
  */
/**
 * After the full DOM has been ,loaded
*/
window.onload = function() {
	// The handlers for buttons
 	$('aboutButton').onclick = openhelp;
	$('helpButton').onclick = openhelp;
	$('rulesButton').onclick = openhelp; 
	$('generateButton').onclick = generate;
	
	$('hintbutton').onclick = getHint;
	$('derivationbutton').onclick = getDerivation;
	$('nextbutton').onclick = getNext;
	$('readybutton').onclick = getReady;
	$('submitbutton').onclick = getFeedback;
	$('readybutton').onclick = getReady;
	$('undobutton').onclick = goBack;
	$('forwardbutton').onclick = goForward;
	
	$('closehelpButton').onclick = closehelp;
	$('closeaboutButton').onclick = closehelp;
	$('closerulesButton').onclick = closehelp; 
	
	$('clearbutton').onclick = clearFeedback; 
	$('copybutton').onclick = copy; 
	
	// the back and forward button  and the copy button are invisable
	setInvisible($('undobutton'));
	setInvisible($('forwardbutton'));
	setInvisible($('copybutton'));

	generate();
}