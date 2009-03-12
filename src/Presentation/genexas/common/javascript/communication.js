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
	$('feedback').innerHTML = "";
	// call a Strategytool-service
	ss_getRemaining(state, function(number) {
			$('progress').update('Steps<br> ' + number); 
			// take a snapshot to be able to go back later
			historyKeeper.newSnapshot(state);
		} );
	// adjust the dimensions of page elements to the generated exercise
	adjustHeight($('exercise'), task, 40, 40);
	adjustRows($('work'), task, 40);
}
/**
 * React on the hint button
 */
function getHint() {
	ss_getHint(snapshot.get('location'), snapshot.get('state'), displayHint);
}
function displayHint(hint) {
	closeallhelp();	
	var expression = (snapshot.get('state')).exercise;
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	if (hint[0]) {
		text +=   '<p><strong>' + hint[1] + '</strong></p>';
	}
	else {
		text +=  '<p>' + sorry + ' <strong>' + expression + '</strong></p>';
	}
	$('feedback').update(text);
	$('feedback').scrollTop = $('feedback').scrollHeight;
	
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
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	if (valid) {
		text += '<p><strong>' + rule + ' rule</strong></p><p>' + resulting + ' <strong>' + nextExpression + '</strong></p><p>' + paste + '</p><p';
		var copyContent = new CopyContent(state, state[3]);
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		historyKeeper.addFeedback();
		historyKeeper.addCopy(copyContent);
	}
	else {
		text +=   + '<p>' + sorry + ' <strong>' + expression + '</strong></p>';	
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		historyKeeper.addFeedback();
	}
	
}
/**
 * React to the derivation button
 */
function getDerivation() {
	ss_getDerivation(snapshot.get('state'), displayDerivation);
 }
 function displayDerivation(setOfRules) {
	var counter = 0;
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	text += derivationtext +  $('work').value + '<br><br>';
	while (counter < setOfRules.length) {
		var rule = setOfRules[counter];
		++counter;
		text += '<font size="+2">\u21D4</font>  <strong>' + rule.name;
		text += '</strong><br>';
		text += rule.expression;
		text += '<br><br>';
	}
	$('feedback').update(text);
	$('feedback').scrollTop = $('feedback').scrollHeight;
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
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	text +=  '<p><strong>' + result[1] + '</strong></p>';
	if (result[0]) {
		$('feedback').update(text);
		$('history').update($('history').innerHTML + '<br><font size="+2">\u21D4</font>&nbsp;&nbsp;&nbsp; ' + state.exercise);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		ss_getRemaining(state, function(number) {$('progress').innerHTML = 'Steps<br> ' + number; historyKeeper.update(state);});
	}
	else {
		text = text + '<p>' + copybutton +  '</p>';
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		setVisible($('copybutton'));
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
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	if (solved) {
		text += '<p>' + yes + ', <strong>' + expression + '</strong> is ' + ready + '.</p>';
	}
	else {
		text += '<p>' + no + ', <strong>' + expression + '</strong> is <strong>' + not + '</strong> ' + ready + '.</p>';
	}
	$('feedback').update(text);
	$('feedback').scrollTop = $('feedback').scrollHeight;
	historyKeeper.addFeedback();
}
