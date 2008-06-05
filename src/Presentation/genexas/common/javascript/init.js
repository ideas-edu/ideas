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
	(areas.feedbackArea).innerHTML = "";
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

/**
 * The areas 
 * exerciseArea: the original a exercise; stays the same until a new exercise is generated
 * workArea: the area where the user rewrites expressions
 * feedbackArea: conmtains the feedback
  * historyArea: contains the sequence of valid rewritings
 */
 function Areas(exercise, work, feedback, history, current) {
	this.exerciseArea = exercise;
	this.workArea = work;
	this.feedbackArea = feedback;
	this.historyArea = history;
}
var areas;
/* * 
 *Undo functionality
*/
/**
 * State is the same datatype as is used in the services
  * Note: exercise is in ASCII form, exectly how we got it from service.cgi
  */
function State(id, prefix, exercise, simpleContext) {
	this.id = id;
	this.prefix = prefix;
	this.exercise = exercise;
	this.simpleContext = simpleContext;
}
/**
 * a historyObject contains:
 * - exercise. Is the exercise that is to be solved, and will stay the same until a new exercise is generated
 * - feedback: Is the content of the feedback area
 * - history: Is the content of the History area
 *  -work: is the content of the work area; 
 *  -copy: is the content of the copy button; this is a combination of a state and a location
 * - state, which contains, the id of the exercise, the prefix, the current *valid* expression, and the simpleContext
 * - location
 * 
 * snapshot is the current historyObject
  */
 /**
  * When the user has asked a possible next step, we receive a state and a location. 
  * Both are held available for the copy button.
  */
function CopyContent(state, location) {
	this.state = state;
	this.location = location;
}
function HistoryObject(exercise, feedback, history, work, copy, state, location) {
	this.exercise = exercise;
	this.feedback = feedback;
	this.history = history;
	this.work = work;
	this.copy = copy;
	this.state = state;
	this.location = location;
}
var snapshot;
/**
 * Our historykeeper will be an array filled with historyObjects
  * statePointer points out the index of the snapshot within the historyList
  */
var HistoryKeeper = function() {
	this.historyList = new Array();
	snapshot = new HistoryObject("", "", "", "", new State("", "", "", ""),  new State("", "", "", ""));
	this.historyList.push(snapshot);
	this.statePointer = 0;
}
var historyKeeper = new HistoryKeeper();

function newSnapshot(exercise, feedback, history, work, copy, state, location) {
	//create a new snapshot
	snapshot = new HistoryObject(exercise, feedback, history, work, copy, state, location);
	historyKeeper.historyList.push(snapshot);
	++ (historyKeeper.statePointer);
	if (historyKeeper.statePointer == 1) {
		show($('undobutton'));
	}
	
}
/**
 * The current snapshot becomes history, and we create a new snapshot with the new content of the feedback area.
*/
function addFeedback(newFeedback) {
	newSnapshot(snapshot.exercise, newFeedback, snapshot.history, snapshot.work, snapshot.copy, snapshot.state, snapshot.location);
}
/**
 * If the expression within the work area differs from thew one in the snapshot,
 * we create a new snapshot with the current value of the work area.
 */
function addWorkexpression(expression) {
	if ((snapshot.work).htmlToAscii() != expression) {
		newSnapshot(snapshot.exercise, snapshot.feedback, snapshot.history, expression, snapshot.copy, snapshot.state, snapshot.location);
	}		
}
/**
 * If the copy content differs from the copy content within the newest state,
  * we create a new state and remember it.
   * Otherwise, we do nothing
 */
function addCopy(newState, newLocation) {
	if ((((snapshot.copy).state).exercise).htmlToAscii() != expression) {
		var newCopy = new CopyContent(newState, newLocation);
		newSnapshot(snapshot.exercise, snapshot.feedback, snapshot.history, snapshot.work, newCopy, snapshot.state, snapshot.location);
	}		
}
function goBack() {
	if (historyKeeper.statePointer > 0) {
		-- (historyKeeper.statePointer);
		snapshot = historyKeeper.historyList[historyKeeper.statePointer];
		fillAreas(snapshot);
		if (historyKeeper.statePointer == 0) {
			hide($('undobutton'));
		}
		show($('forwardbutton'));
	}
}
 function goForward() {
	if ((historyKeeper.statePointer +1) < historyKeeper.historyList.length) {
		++(historyKeeper.statePointer);
		snapshot = historyKeeper.historyList[historyKeeper.statePointer];
		fillAreas(snapshot);
		if ((historyKeeper.stateCounter + 1) == historyKeeper.historyList.length) {
			hide($('forwardbutton'));
		}
		show($('undobutton'));
	}
	else {
		alert("You can't move forward unless you have been there!");
	}
}

function fillAreas(stateObject) {
	(areas.exerciseArea).innerHTML = stateObject.exercise;
	(areas.workArea).value = (stateObject.work).htmlToAscii();
	(areas.feedbackArea).innerHTML = stateObject.feedback;
	(areas.historyArea).innerHTML = stateObject.history;
}
function copy() {
	var workArea  = areas.workArea;
	workArea.value = ((snapshot.copy).state).exercise;
	addWorkexpression(((snapshot.copy).state).exercise);
}
function hide(element) {
	element.style.visibility = 'hidden';
}
function show(element) {
	element.style.visibility = 'visible';
}

function writeArray(list) {
	elements = "";
	for (var i = 0; i < list.length; ++i) {
		elements = elements + list[i] + ",<br>";
	}
	return elements;
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
	
	// the areas
	areas = new Areas($('exercise'), $('work'), $('feedback'), $('history'), $('current'));
}