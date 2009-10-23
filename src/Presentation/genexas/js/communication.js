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
	adjustHeight($('exercise'), task, 40, 40);
	adjustRows($('work'), task, 40);
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
        var s = historyKeeper.historyList[historyKeeper.historyList.length-1];
	onefirsttextService('hint button', s.get('state'), displayHint);
}
function displayHint(rule, valid, state) {
	closeallhelp();
	var s = historyKeeper.historyList[historyKeeper.historyList.length-1];
	var expression = (s.get('state')).exercise;
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
        var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	onefirsttextService('step button', s.get('state'), displayNext);
 }
 function displayNext(rule, valid, state) {
	var nextExpression = (state.exercise).asciiToHtml() ;
	var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	var expression = ((s.get('state')).exercise).asciiToHtml();
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
     var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
     derivationtextService('worked-out exercise button', s.get('state'), displayDerivation);
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

     var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
     var counter = 0;
     var newText = '<strong>Worked-out exercise</strong><br><br>' + s.get('state').exercise + '<br>';
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
	var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	submittextService(caller, s.get('state'), workExpression, displayFeedback);
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
	var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	readyService('ready button', s.get('state'), handleSolved);
}
function handleSolved(solved) {
	var s = historyKeeper.historyList[historyKeeper.historyList.length - 1];
	var expression = (s.get('state')).exercise;
	var newText = '';

	if (solved) {
		newText = '<p>Yes, <strong>' + expression + '</strong> is solved.</p>';
	}
	else {
		newText = '<p>No, <strong>' + expression + '</strong> is <strong>not</strong> solved.</p>';
	}

	addToFeedback(newText);
        historyKeeper.addFeedback();
}
