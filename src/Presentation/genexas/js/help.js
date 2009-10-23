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

function State(code, prefix, term, context) {
   this.code    = code;
   this.prefix  = prefix;
   this.term    = term;
   this.context = context;
}

var history = new Object();
history.historyList = new Array();

history.clear = function() {
   history.historyList = new Array();
}

history.addState = function (state) {
   history.historyList.push(state);
}

function currentState() {
   return history.historyList.last();
}

function goBack() {
  if (history.historyList.length > 1  ) {
    var state = currentState();
    if ($('work').value == state.term) { // student didn't touch expression
      history.historyList.pop();
    } 
  }      
  fillAreas();
}

function fillAreas() {
   var state = currentState();
   $('work').value = state.term;
   updateDerivation();
}

function updateDerivation() {
   var i = 0;
   var text = '';
   while (i < history.historyList.length) {
      if (i!=0) {
         text += '<br><font size="+2">\u21D4</font>&nbsp;&nbsp;&nbsp;';
      }
      var state = history.historyList[i];
      text += state.term;
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
         history.addState(state);
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