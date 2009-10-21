/**
  * Global variable: keepFeedback
  * made global, so we wont have to check the value of the radiobutton each time
  */
var keepFeedback = false;
/**
  * functions to choose between keepFeedback true or false
  */
function setClearFeedback() {
	 if ($('feedbackclearchoice').checked == true) {
	    keepFeedback = false;
	    $('clearbutton').hide();
	 } else {
	   setKeepFeedback();
	 }
}
function setKeepFeedback() {
	keepFeedback = true;
	$('clearbutton').show();
}

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
	element.setStyle({height: length + 'px'});
}

/***
 * adjust the number of rows  with respect to the length of the expression
 * row is the number of characters in a row
 */
 function adjustRows(element, expression, rows) {
	var length = expression.length;
	length /= rows;
	length = Math.floor(length) + 2;
	element.rows =  length ;
}

/**
 * From HTML characters to ascii and back
  */ 
String.prototype.htmlToAscii = function() {
    var s = this.replace(/\\/g, '\\\\');
    return s;
}

String.prototype.asciiToHtml = function() {
	var resultstring = this.replace(/>/g, '&gt;');
	resultstring = resultstring.replace(/</g, '&lt;');
	resultstring = resultstring.replace(/\\\\/g, '\\');
	 return resultstring;
}
/**
* produce text in HTML, on seperate lines.
*/
function writeArray(list) {
	elements = "";
	for (var i = 0; i < list.length; ++i) {
		elements = elements + list[i] + ",<br>";
	}
	return elements;
}

/**
 * clear the feedback area
 */
function clearFeedback() {
	$('feedback').update('');
}
function parse(json){
    try{
        if(/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.test(json)){
            var j = eval('(' + json + ')');
            return j;
		}
	}catch(e){
    }
    throw new SyntaxError('parseJSON');
}

/**
 * Een datatype voor regels en de expressie die het resultaat is van het toepassen van die regel
 */
 function Rule(name, location, expression) {
	this.name = name;
	this.location = location;
	this.expression = expression;
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
 * a snapshot contains:
 * - exercise. Is the exercise that is to be solved, and will stay the same until a new exercise is generated
 * - feedback: Is the content of the feedback area
 * - history: Is the content of the History area
 *  -work: is the content of the work area; 
 *  -copy: is the content of the copy button; this is a combination of a state and a location
 * - state, which contains, the id of the exercise, the prefix, the current *valid* expression, and the simpleContext
 * - location
 *  steps
 * 
 * snapshot is the current snapshot
 * it is a Hash object, so we can add new key-value pairs if necessary
  */
var snapshot;
/**
 * Our historykeeper will be an array filled with snapshot, containing the variable contents of the page elements
  * snapshotPointer points out the index of the current snapshot within the historyList
  */
var historyKeeper = new Object();
historyKeeper.historyList = new Array();
historyKeeper.snapshotPointer = -1;
/**
 * function that clears all memorized state
  */
historyKeeper.clear = function() {
	historyKeeper.historyList = new Array();
	historyKeeper.snapshotPointer = -1;
}
/**
 * function to add the current snapshot to the history 
 */
historyKeeper.addSnapshot = function (snapshot) {
	historyKeeper.historyList.push(snapshot);
	++historyKeeper.snapshotPointer;
	if (historyKeeper.snapshotPointer >= 1) {
		$('undobutton').show();
	}
	// clear any state under the copy button ( it will be outdated)	
	historyKeeper.removeCopy();
}
/**
 * create a new snapshot, based on the values of the page elements
 */
historyKeeper.newSnapshot = function (state) {
	if (snapshot) {
		// clear the state under 'copy'
		if (snapshot.get('copy')) {
			snapshot.unset('copy');
		}
		var newSnapshot = snapshot.clone();
		snapshot = newSnapshot;
	}
	else {
		snapshot = new Hash();
		snapshot.set('location', new Array());
		snapshot.set('exercise', $('exercise').innerHTML);
	}
	snapshot.set('feedback', $('feedback').innerHTML);
	snapshot.set('history', $('history').innerHTML);
	snapshot.set('work', $('work').value);
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
historyKeeper.addCopy = function(copycontent) {
	snapshot.set('copy', copycontent);
}
historyKeeper.removeCopy = function() {
	snapshot.unset('copy');
}
 /**
  * When the user has asked a possible next step, we receive a state and a location. 
  * Both are held available for the copy button.
  * This function returns an object to hold the state and the location
  */
function CopyContent(state, location) {
	this.state = state;
	this.location = location;
}
function goBack() {
  if (historyKeeper.historyList.length > 1  ) {
    var stateObject = historyKeeper.historyList[historyKeeper.historyList.length-1];
    if ($('work').value == stateObject.get('state').exercise) { // student didn't touch expression
      -- (historyKeeper.snapshotPointer);
      historyKeeper.historyList.pop();
    } 
  }      
  fillAreas();
}

function fillAreas() {
        var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	$('work').value = s.get('state').exercise;
        updateDerivation();
}

function updateDerivation() {
   var i = 0;
   var text = '';
   while (i < historyKeeper.historyList.length) {
      if (i!=0) {
         text += '<br><font size="+2">\u21D4</font>&nbsp;&nbsp;&nbsp;';
      }
      var state = historyKeeper.historyList[i];
      text += state.get('state').exercise;
      i++;
   }
   $('history').update(text);
   $('history').scrollTop = $('history').scrollHeight;
}

// will be auto step
// BHR: move this function to other button handlers, and rename
function copy() {
        var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
        onefirsttextService('auto step button', s.get('state'), autoHandler);
}
function autoHandler(rule, valid, state) {
        addToFeedback('<strong>Auto step: </strong>' + rule + ' (has been done)');
	if (valid) {
   	   historyKeeper.newSnapshot(state);
	   historyKeeper.snapshotPointer++;
	   fillAreas();
        }
}


/* 
 * Menubuttons for help, about and a set of rewriting rules.
 * There are default files in common; there may be specific files for each kind of exercise.
 */
function openhelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'rulesButton':
        ($('help')).className='helparea invisible';
		($('about')).className='helparea invisible';
		($('rules')).className='helparea visible';
		break;
    case 'helpButton':
        ($('rules')).className='helparea invisible';
		($('about')).className='helparea invisible';
		($('help')).className='helparea visible';
		break;
    case 'aboutButton':
       ($('help')).className='helparea invisible';
		($('rules')).className='helparea invisible';
		($('about')).className='helparea visible';
	}
}
function closehelp(e)
{
	if (!e) var e = window.event;
	switch (this.id) {
    case 'closerulesButton':
		($('rules')).className='helparea invisible';
		break;
    case 'closehelpButton':
		($('help')).className='helparea invisible';
		break;
    case 'closeaboutButton':
		($('about')).className='helparea invisible';
	}
}
function closeallhelp() {
	($('rules')).className='helparea invisible';
	($('help')).className='helparea invisible';
	($('about')).className='helparea invisible';
}
