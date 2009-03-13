/**
 * Generate a new exercise,
 * A function to call the service and a display function
 */
 function generate() {
	ss_generate(5, displayExercise);
}
 /**
  * Display new exercise.
  * Updates the exercise area, the work area and the history area.
  * A new snapshot is taken for the back button
  */
function displayExercise(state) {
	closeallhelp();
	historyKeeper.clear();
	//setInvisible($('copybutton'));
	var task = state.exercise;
	$('exercise').update(task);
	$('work').value = task;
	$('history').update(task);
//	$('feedback').innerHTML = "";
	// call a Strategytool-service
/*	ss_getRemaining(state, function(number) {
			$('progress').update('Steps<br> ' + number); 
			// take a snapshot to be able to go back later
			historyKeeper.newSnapshot(state);
		} );
*/		
	historyKeeper.newSnapshot(state);
	// adjust the dimensions of page elements to the generated exercise
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

function getAuto() {
   alert('auto step');
}


/**
 * React on the hint button
 */
function getHint() {
	ss_getHint(snapshot.get('location'), snapshot.get('state'), displayHint);
}
function displayHint(hint) {
	closeallhelp();	
	var expression = presenteertekst((snapshot.get('state')).exercise);
	var newText = '';
 
	if (hint[0]) {
		newText =   '<p><strong>' + hint[1] + '</strong></p>';
	}
	else {
		newText =  '<p>' + sorry + ' <strong>' + expression + '</strong></p>';
	}
        addToFeedback(newText);
	historyKeeper.addFeedback();
}
/**
 * React to the next button
 */
function getNext() {
	ss_getNext(snapshot.get('state'), displayNext);
 }
 function displayNext(rule, valid, state) {
	var nextExpression = (state.exercise).asciiToHtml() ;
	var expression = ((snapshot.get('state')).exercise).asciiToHtml();
	var newText = '';

	if (valid) {
		newText = '<p><strong>' + rule + ' rule</strong></p><p>' + resulting + ' <strong>' + nextExpression + '</strong></p><p>' + paste + 
'</p><p';
        }
        else {
                newText = '<p>' + sorry +  ' <strong>' + expression + '</strong></p>';
        }

        addToFeedback(newText);

        if (valid) {
                var copyContent = new CopyContent(state, state[3]);
		historyKeeper.addFeedback();
		historyKeeper.addCopy(copyContent);
	}
	else {
		historyKeeper.addFeedback();
	}
	$('history').scrollTop = $('history').scrollHeight;
}
/**
 * React to the derivation button
 */
function getDerivation() {
	ss_getDerivation(snapshot.get('state'), displayDerivation);
 }
 function displayDerivation(setOfRules) {
	var counter = 0;
	var newText = '<strong>' + derivationtext + '</strong><br><br>' + $('work').value + '<br>';
	while (counter < setOfRules.length) {
		var rule = setOfRules[counter];
		++counter;
		newText += '<font size="+2">&nbsp;&nbsp;&nbsp;\u21D4</font>  <strong>' + rule.name;
		newText += '</strong><br>';
		newText += rule.expression;
	        newText += '<br>';
	}
	addToFeedback(newText);
	historyKeeper.addFeedback();
}
 /**
 * React to the submit button
 */
 function getFeedback() {
	var workExpression = (($('work')).value).htmlToAscii();
	ss_getFeedback(snapshot.get('state'), workExpression, displayFeedback);
 }
function displayFeedback(result, state) {
	// always paste the result
	var newText = '<p>' + result[1] + '</p>';
        if (!result[0]) {
           newText += '<p>' + copybutton + '</p';
        }
        addToFeedback(newText);

	if (result[0]) {
		$('history').update($('history').innerHTML + '<br><font size="+2">\u21D4</font>&nbsp;&nbsp;&nbsp; ' + state.exercise);
		// this call to remaining steps has a side-effect
                ss_getRemaining(state, function(number) {$('progress').innerHTML = 'Steps<br> ' + number; historyKeeper.update(state);});
	}
	else {
//		setVisible($('copybutton'));
	}
}
/**
 * React to the Ready button
*/
function getReady() {
	ss_getReady(snapshot.get('state'), handleSolved);
}
function handleSolved(solved) {
	var expression = (snapshot.get('state')).exercise;
	var newText = '';

	if (solved) {
		newText = '<p>' + yes + ', <strong>' + expression + '</strong> is ' + ready + '.</p>';
	}
	else {
		newText = '<p>' + no + ', <strong>' + expression + '</strong> is <strong>' + not + '</strong> ' + ready + '.</p>';
	}

	addToFeedback(newText);
        historyKeeper.addFeedback();
}
