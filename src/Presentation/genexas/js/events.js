/****************************************************
 * Event handlers for Hint Buttons
 */
 
function onBackClick() {
   if (history.historyList.length > 1  ) {
      var state = currentState();
      if ($('work').value == state.term) { // student didn't touch expression
         history.historyList.pop();
      } 
   }      
   fillAreas();
}

function onReadyClick() {
   var state = currentState();
   
   function callback(solved) {
      var expression = state.term;
      var newText = '';
      if (solved) {
         newText = '<p>Yes, <strong>' + expression + '</strong> is solved.</p>';
      } else {
         newText = '<p>No, <strong>' + expression + '</strong> is <strong>not</strong> solved.</p>';
      }
    addToFeedback(newText); };

   readyService('ready button', state, callback);
}

function onSubmitClick() {
   // Call generalized event handler
   onSubmit('submit button');
}
 
function onSubmit(caller) {
   var workExpression = (($('work')).value).htmlToAscii();
   var state = currentState();
   
   function callback(result, state) {
      var newText = '<p>' + result[1] + '</p>';
      addToFeedback(newText);
      if (result[0]) {
         history.addState(state);
         updateDerivation();
      } };
   
   submittextService(caller, state, workExpression, callback);
}

function onHintClick() {
   var state = currentState();
   
   function callback(rule, valid, state) {
      closeallhelp();
      var expression = state.term;
      var newText = '';
      if (valid) {
         newText =   '<p><strong>' + rule + '</strong></p>';
      } else {
         newText =  '<p>Sorry, there is no rule applicable to <strong>' + expression + '</strong></p>';
      }
      addToFeedback(newText); };

   onefirsttextService('hint button', state, callback);
}

function onStepClick() {
   var state = currentState();
   
   function callback(rule, valid, newState) {
      var nextExpression = (newState.term).asciiToHtml() ;
      var expression = (state.term).asciiToHtml();
      var newText = '';
      if (valid) {
         newText = '<p>' + rule + ' </p><p>The result would be <strong>' + nextExpression + 
            '</strong></p><p>Press the <strong>Auto step</strong> button to execute this step automatically.</p><p';
      } else {
         newText = '<p>Sorry, there is no rule applicable to <strong>' + expression + '</strong></p>';
      }
    addToFeedback(newText); };

   onefirsttextService('step button', state, callback);
}

function onAutoStepClick() {
   var state = currentState();

   function callback(rule, valid, state) {
      addToFeedback('<strong>Auto step: </strong>' + rule + ' (has been done)');
      if (valid) {
         history.addState(state);
         fillAreas();
      } };
   
   onefirsttextService('auto step button', state, callback);
}

function onWorkedOutClick() {
   var state = currentState();

   function callback(result) {
      var setOfRules = new Array();
      var counter = 0;
      while (counter < result.length) {
         var entry = result[counter];
         var appliedRule = new Rule(entry[0], null, entry[1]);
         ++counter;
         setOfRules.push(appliedRule);
      }

      var counter = 0;
      var newText = '<strong>Worked-out exercise</strong><br><br>' + state.term + '<br>';
      while (counter < setOfRules.length) {
         var rule = setOfRules[counter];
         ++counter;
         newText += '<font size="+2">&nbsp;&nbsp;&nbsp;\u21D4</font>  <strong>' + rule.name;
         newText += '</strong><br>';
         newText += rule.expression;
         newText += '<br>';
      }
   addToFeedback(newText); };

   derivationtextService('worked-out exercise button', state, callback);
}

/****************************************************
 * Event handlers for other buttons
 */

function onNewExerciseClick(state) {
   // Call generalized event handler
    onNewExercise('new button');
    onClear();
}

function onNewExercise(caller) {
   function callback(state) {
      closeallhelp();
      history.clear();
      var task = state.term;
      $('exercise').update(task);
      $('work').value = task;
      $('history').update(task);
      history.addState(state);
   }
	
   generateService(caller, getDifficulty(), callback);
}

function onClearClick() {
   // Call generalized event handler
   onClear();
}

function onClear() {
   $('feedback').update('');
}

function onLastOnlyClick() {
   if (keepFeedback()) {
      $('clearbutton').show();
   } else {
      $('clearbutton').hide();
   }
}