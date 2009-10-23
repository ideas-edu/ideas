function keepFeedback() {
   return $('lastonlychoice').checked == false;
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

function welcomeMessage() {
   var msg = "<p>Welcome to the Exercise Assistant. Rewrite the expression in the work area and hit <strong>Submit</strong> after each step. Press the <strong>Ready</strong> button as soon as your proposition is in disjunctive normal form.</p>";
   msg += "<p>If you are stuck in the exercise, you can use one of the hint buttons: ask for a hint, the next step, automatically perform the next step, or show a worked-out example.</p>";
   msg += "<p>There is a special keyboard to insert the logical symbols, or you can use the <strong>short-keys '-', 'o', 'a', 'i', or '='</strong>.</p>";
   msg += "<p>The Feedback area will be cleared before new feedback is written to the area. If you want to keep all messages, then disable the checkbox.</p>";

   addToFeedback(msg);
}