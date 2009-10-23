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
}
/**
 * create a new snapshot, based on the values of the page elements
 */
historyKeeper.newSnapshot = function (state) {
   if (snapshot) {
      var newSnapshot = snapshot.clone();
      snapshot = newSnapshot;
   }
   else {
      snapshot = new Hash();
   }
   snapshot.set('state', state);
   historyKeeper.addSnapshot(snapshot);
   
}
/**
 * create a new snapshot, based on the values of the page elements
 */
historyKeeper.update = function (state) {
   var newSnapshot = snapshot.clone();
   snapshot = newSnapshot;
   snapshot.set('state', state);
   historyKeeper.addSnapshot(snapshot);
   
}

function currentState() {
   return historyKeeper.historyList[historyKeeper.historyList.length-1].get('state');
}

function goBack() {
  if (historyKeeper.historyList.length > 1  ) {
    var state = currentState();
    if ($('work').value == state.exercise) { // student didn't touch expression
      -- (historyKeeper.snapshotPointer);
      historyKeeper.historyList.pop();
    } 
  }      
  fillAreas();
}

function fillAreas() {
   var state = currentState();
   $('work').value = state.exercise;
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
        var state = currentState();
        onefirsttextService('auto step button', state, autoHandler);
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