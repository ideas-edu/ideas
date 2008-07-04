/* *
  * This file contains the initialization, 
  * some help functions, and
  * several functions for the back and forward buttons
  */
  
 /***
 * adjust the area  with respect to the length of the expression
 * row is the number of characters in a row
 * height is the number of pixels per row
 */
 function adjustHeight(element, expression, row, height) {
	var length = expression.length;
	length /= row;
	length = Math.floor(length) + 1;
	length = length*height;
	element.style.height = length + 'px';
}
 /***
 * adjust the number of rows  with respect to the length of the expression
 * row is the number of characters in a row
 */
 function adjustRows(element, expression, rows) {
	var length = expression.length;
	length /= rows;
	length = Math.floor(length) + 1;
	element.rows =  length ;
}
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
	$('feedback').innerHTML = "";
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
 *  steps
 * 
 * snapshot is the current historyObject
 * it is a Hash object, so we can add new key-value pairs if necessary
  */
var snapshot;
/**
 * Our historykeeper will be an array filled with hashObjects, containing the variable contents of the page elements
  * statePointer points out the index of the snapshot within the historyList
  */
var historyKeeper = new Object();
historyKeeper.historyList = new Array();
historyKeeper.statePointer = -1;
/**
 * function to add the current snapshot to the history 
 */
historyKeeper.addSnapshot = function () {
	historyKeeper.historyList.push(snapshot);
	++historyKeeper.statePointer;
	if (historyKeeper.statePointer >= 1) {
		$('undobutton').show();
	}
}
/**
 * create a new snapshot, based on the values of the page elements
 */
historyKeeper.newSnapshot = function (state) {
	if (snapshot) {
		if (snapshot.get('copy')) {
			snapshot.unset('copy');
		}
		var newSnapshot = snapshot.clone();
		snapshot = newSnapshot;
	}
	else {
		snapshot = new Hash();
		snapshot.set('location', new Array());
	}
	snapshot.set('feedback', $('feedback').innerHTML);
	snapshot.set('history', $('history').innerHTML);
	snapshot.set('work', $('work').value);
	snapshot.set('steps', $('progress').innerHTML);
	snapshot.set('state', state);
	historyKeeper.addSnapshot(snapshot);
	
}
/**
 * create a new snapshot, based on the values of the page elements
 */
historyKeeper.update = function (state) {
	var newSnapshot = snapshot.clone();
	snapshot = newSnapshot;
	snapshot.set('feedback', $('feedback').innerHTML);
	snapshot.set('history', $('history').innerHTML);
	snapshot.set('work', $('work').value);
	snapshot.set('steps', $('progress').innerHTML);
	snapshot.set('state', state);
	historyKeeper.addSnapshot(snapshot);
	
}
/**
 * The current snapshot becomes history, and we create a new snapshot with the new content of the feedback area.
*/
historyKeeper.addFeedback = function () {
	if (snapshot) {
		var newSnapshot = snapshot.clone();
		snapshot = newSnapshot;
		snapshot.set('feedback', $('feedback').innerHTML);
		historyKeeper.addSnapshot(snapshot);
	}
}
 /**
  * When the user has asked a possible next step, we receive a state and a location. 
  * Both are held available for the copy button.
  */
function CopyContent(state, location) {
	this.state = state;
	this.location = location;
}
historyKeeper.addCopy = function(copycontent) {
	snapshot.set('copy', copycontent);
	$('copybutton').show();
}
function goBack() {
	if (historyKeeper.statePointer > 0) {
		-- (historyKeeper.statePointer);
		var stateObject = historyKeeper.historyList[historyKeeper.statePointer];
		fillAreas(stateObject);
		if (historyKeeper.statePointer == 0) {
			$('undobutton').hide();
		}
		$('forwardbutton').show();
	}
}
 function goForward() {
	if ((historyKeeper.statePointer +1) < historyKeeper.historyList.length) {
		++(historyKeeper.statePointer);
		var stateObject = historyKeeper.historyList[historyKeeper.statePointer];
		fillAreas(stateObject);
		if ((historyKeeper.statePointer + 1) == historyKeeper.historyList.length) {
			$('forwardbutton').hide();
		}
		$('undobutton').show();
	}
	else {
		alert("You can't move forward unless you have been there!");
	}
}

function fillAreas(stateObject) {
	$('exercise').innerHTML = stateObject.get('state').exercise;
	$('work').value = (stateObject.get('work')).htmlToAscii();
	$('feedback').innerHTML = stateObject.get('feedback');
	$('history').innerHTML = stateObject.get('history');
	$('progress').innerHTML = stateObject.get('steps');
	adjustHeight($('exercise'), $('exercise').innerHTML, 40, 40);
	adjustRows($('work'), $('work').value, 40);
}
function copy() {
	if (snapshot.get('copy')) {
		$('work').value = snapshot.get('copy').state.exercise;
	}
	else {
		if (snapshot.get('work')) {
			$('work').value = snapshot.get('work') ;
		}
	}
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
	$('undobutton').hide();
	$('forwardbutton').hide();
	$('copybutton').hide();

	generate();
}