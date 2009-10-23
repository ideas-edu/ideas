/**
 * Generate a new exercise,
 * A function to call the service and a display function
 */
function generate(caller) {
   generateService(caller, getDifficulty(), displayExercise);
}

function displayNewExercise(state) {
    generate('new button');
    clearFeedback();
}

// The url for the services
function getURL() {
   return "cgi/service.cgi";
}

function getDifficulty() {
   var diff = 2;
   if ($('Easy').checked) {
     diff = 1;
   } else if ($('Difficult').checked) {
     diff = 3;
   }
   return diff;
}

 /**
  * Display new exercise.
  * Updates the exercise area, the work area and the history area.
  * A new snapshot is taken for the back button
  */
function displayExercise(state) {
   closeallhelp();
   historyKeeper.clear();
   var task = state.exercise;
   $('exercise').update(task);
   $('work').value = task;
   $('history').update(task);
   historyKeeper.newSnapshot(state);
}

function addToFeedback(newText) {
   var text = '';
   if (keepFeedback) {
      text = $('feedback').innerHTML;
      if (text != '') {
         text += '<hr>';
      }
   }
   text += newText;

   $('feedback').update(text);
   $('feedback').scrollTop = $('feedback').scrollHeight;
}

/**
 * React on the hint button
 */
function getHint() {
   var state = currentState();
   onefirsttextService('hint button', state, displayHint);
}
function displayHint(rule, valid, state) {
   closeallhelp();
   var state = currentState();
   var expression = state.exercise;
   var newText = '';
   if (valid) {
      newText =   '<p><strong>' + rule + '</strong></p>';
   }
   else {
      newText =  '<p>Sorry, there is no rule applicable to <strong>' + expression + '</strong></p>';
   }
        addToFeedback(newText);
}
/**
 * React to the next button
 */
function getNext() {
   var state = currentState();
   onefirsttextService('step button', state, displayNext);
}

function displayNext(rule, valid, newState) {
   var oldState = currentState();
   var nextExpression = (newState.exercise).asciiToHtml() ;
   var expression = (oldState.exercise).asciiToHtml();
   var newText = '';

   if (valid) {
      newText = '<p>' + rule + ' </p><p>The result would be <strong>' + nextExpression + 
         '</strong></p><p>Press the <strong>Auto step</strong> button to execute this step automatically.</p><p';
        }
        else {
                newText = '<p>Sorry, there is no rule applicable to <strong>' + expression + '</strong></p>';
        }
        addToFeedback(newText);
}

/**
 * React to the derivation button
 */
function getDerivation() {
     var state = currentState();
     derivationtextService('worked-out exercise button', state, displayDerivation);
 }
 function displayDerivation(result) {
       var setOfRules = new Array();
       var counter = 0;
       while (counter < result.length) {
               var entry = result[counter];
                var appliedRule = new Rule(entry[0], null, entry[1]);
               ++counter;
               setOfRules.push(appliedRule);
            }

     var state = currentState();
     var counter = 0;
     var newText = '<strong>Worked-out exercise</strong><br><br>' + state.exercise + '<br>';
   while (counter < setOfRules.length) {
      var rule = setOfRules[counter];
      ++counter;
      newText += '<font size="+2">&nbsp;&nbsp;&nbsp;\u21D4</font>  <strong>' + rule.name;
      newText += '</strong><br>';
      newText += rule.expression;
           newText += '<br>';
   }
   addToFeedback(newText);
}
 /**
 * React to the submit button
 */
 function getFeedbackButton() {
    getFeedback('submit button');
 }
 
 function getFeedback(caller) {
   var workExpression = (($('work')).value).htmlToAscii();
   var state = currentState();
   submittextService(caller, state, workExpression, displayFeedback);
 }
function displayFeedback(result, state) {
   var newText = '<p>' + result[1] + '</p>';
        addToFeedback(newText);
   if (result[0]) {
         historyKeeper.newSnapshot(state);
         updateDerivation();
   }
}
/**
 * React to the Ready button
*/
function getReady() {
   var state = currentState();
   readyService('ready button', state, handleSolved);
}
function handleSolved(solved) {
   var state = currentState();
   var expression = state.exercise;
   var newText = '';

   if (solved) {
      newText = '<p>Yes, <strong>' + expression + '</strong> is solved.</p>';
   }
   else {
      newText = '<p>No, <strong>' + expression + '</strong> is <strong>not</strong> solved.</p>';
   }

   addToFeedback(newText);
}
