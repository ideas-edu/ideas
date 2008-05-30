/* *
  * This file contains the initialization, 
  * some help functions, and
  * several functions for the back and forward buttons
  */
/**
 * From HTML characters to ascii and back
  */
String.prototype.htmlToAscii = function() {
	var resultstring = this.replace(/&gt;/g, '>');
	resultstring = resultstring.replace(/&lt;/g, '<');
	resultstring = resultstring.replace(/\\/g, '\\\\');
	 return resultstring;
}
String.prototype.asciiToHtml = function() {
	var resultstring = this.replace(/>/g, '&gt;');
	resultstring = resultstring.replace(/</g, '&lt;');
	resultstring = resultstring.replace(/\\\\/g, '\\');
	 return resultstring;
}

function clearFeedback() {
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	feedbackArea.innerHTML = "";
	addToHistory(exerciseArea.innerHTML, workArea.value, "", historyArea.innerHTML);
}
function parse(json){
    try{
        if(/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.test(json)){
            var j = eval('(' + json + ')');
            return j;
		}
	}catch(e){
    }
    throw new SyntaxError("parseJSON");
}

/* * 
 *Undo functionality
*/
var state = new Array();
var stateCounter = 0;
var copyContent = "";


function addToHistory(exercise, work, feedback, history, current) {
	var historyObject = new Object();
	historyObject.exercise = exercise;
	historyObject.work = work;
	historyObject.feedback = feedback;
	historyObject.history = history;
	historyObject.current = current;
	++ stateCounter;
	if (stateCounter == 1) {
		show($('undobutton'));
	}
	state.push(historyObject);
}
function goBack() {
	if (stateCounter > 0) {
		-- stateCounter;
		var stateObject = state[stateCounter];
		fillAreas(stateObject);
		if (stateCounter == 0) {
			hide($('undobutton'));
		}
		show($('forwardbutton'));
	}
}
function goForward() {
	if ((stateCounter +1) < state.length) {
		++stateCounter;
		var stateObject = state[stateCounter];
		fillAreas(stateObject);
		if ((stateCounter + 1) == state.length) {
			hide($('forwardbutton'));
		}
		show($('undobutton'));
	}
	else {
		alert("You can't move forward unless you have been there!");
	}
}
function fillAreas(stateObject) {
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	var current = $('current');
	exerciseArea.innerHTML = stateObject.exercise;
	workArea.value = stateObject.work;
	//feedbackArea.innerHTML = stateObject.feedback;
	historyArea.innerHTML = stateObject.history;
	current.innerHTML = stateObject.current;
}
function copy() {
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	var current = $('current');
	workArea.value = copyContent;
	addToHistory(exerciseArea.innerHTML, workArea.value, feedbackArea.innerHTML, historyArea.innerHTML, current.innerHTML)
	hide($('copybutton'));
}
function hide(element) {
	element.style.visibility = 'hidden';
}
function show(element) {
	element.style.visibility = 'visible';
}
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
	$('nextbutton').onclick = getNext;
	$('progressbutton').onclick = getRemaining;
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
	
	// the back and forward button are invisable
	hide($('undobutton'));
	hide($('forwardbutton'));
	hide($('copybutton'));
	// the elements that we need everywhere
	
	// the state
	var historyObject = new Object();
	historyObject.exercise = "";
	historyObject.work = "";
	historyObject.feedback = "";
	historyObject.history = "";
	historyObject.current = "";
	state.push(historyObject);
  // if this is the first time we have
  // loaded the page...
}