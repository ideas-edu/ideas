
/* *
  * This file contains the initialisation
  */

document.observe("dom:loaded", init);

function init() {

	addEventSimple($("work"), 'keypress', controleer);

 	$('aboutButton').observe('click', openhelp);
	$('generateButton').observe('click', displayNewExercise);
	
	$('hintbutton').observe('click', getHint);
	$('derivationbutton').observe('click', getDerivation);
	$('nextbutton').observe('click', getNext);
	$('readybutton').observe('click', getReady);
	$('submitbutton').observe('click', getFeedbackButton);
	$('readybutton').observe('click', getReady);
	$('undobutton').observe('click', goBack);
	
	$('closehelpButton').observe('click', closehelp);
	$('closeaboutButton').observe('click', closehelp);
	$('closerulesButton').observe('click', closehelp); 
	
	$('clearbutton').observe('click', clearFeedback); 
	$('feedbackclearchoice').observe('click', setClearFeedback); 
	$('copybutton').observe('click', copy); 

  $('work').focus();
	generate('init');
}

						
