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
	
	setInvisible($('copybutton'));
	var task = state.exercise;
	$('exercise').update(task);
	$('work').value = task;
	$('history').update(task);
	ss_getRemaining(state, function(number) {
			$('progress').update('Steps<br> ' + number); 
			historyKeeper.newSnapshot(state);
		} );
	adjustHeight($('exercise'), task, 40, 40);
	adjustRows($('work'), task, 40);
}
/**
 * React on the hint button
 */
function getHint() {
	ss_getHint(snapshot.get('location'), snapshot.get('state'), displayHint);
}
function displayHint(listOfRules) {
	closeallhelp();	
	var expression = (snapshot.get('state')).exercise;
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	if (listOfRules.length > 0) {
		rules = writeArray(listOfRules);
		text +=   '<p>' + applicable + ' <strong>' + expression + '</strong><br>is:<br><br><strong>' + rules + '</strong></p>';
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
	// Does the work area contain the last valid step?
	if (checkWorkArea()) {
		$('feedback').update($('feedback').innerHTML + '<p>' + changed + '</p>');
		setVisible($('copybutton'));
		historyKeeper.addFeedback();
	}
	else {
		ss_getNext(snapshot.get('state'), displayNext);
	}
 }
 function displayNext(rule, location, state) {
	var nextExpression = (state.exercise).asciiToHtml() ;
	var expression = ((snapshot.get('state')).exercise).asciiToHtml();
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	if (rule) {
		text += '<p>' + applicable + ' <strong>' + expression + '</strong>:<br><br><strong>' + rule + ' rule</strong></p><p>' + resulting + ' <strong>' + nextExpression + '</strong></p><p>' + paste + '</p><p';
		var copyContent = new CopyContent(state, location);
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		historyKeeper.newSnapshot(state);
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
	// Does the work area contain the last valid step?
	if (checkWorkArea()) {
		$('feedback').update($('feedback').innerHTML + '<p>' + changed + '</p>');
		setVisible($('copybutton'));
		historyKeeper.addFeedback();
	}
	else {
		ss_getDerivation(snapshot.get('state'), displayDerivation);
	}
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
		text += 'Application of:  <strong>' + rule.name;
		text += '</strong><br>results in: <br>';
		text += rule.expression;
		text += '<br><br>';
	}
	$('feedback').update(text);
	$('feedback').scrollTop = $('feedback').scrollHeight;
	historyKeeper.addFeedback();
}
/**
 * Check whether the work area contains the last valid step
 */
 function checkWorkArea() {
	var workExpression = ($('work').value).htmlToAscii();
	var result = false;
	if (! workExpression.startsWith((snapshot.get('work').htmlToAscii()))) {
		result = true;
	}
	return result;
 }
 /**
 * React to the submit button
 */
 function getFeedback() {
	var workExpression = (($('work')).value).htmlToAscii();
	// Does the work area contain the last valid step?
	 if (! checkWorkArea()) {
		$('feedback').update($('feedback').innerHTML + '<p>' + unchanged + '</p>');
		historyKeeper.addFeedback();
	}
	else {
		ss_getFeedback(snapshot.get('state'), workExpression, displayFeedback);
	} 
}
function displayFeedback(result, rules, state) {
	// always paste the result
	var text = '';
	if (keepFeedback) {
		text = $('feedback').innerHTML ;
	}
	text +=  '<p><strong>' + result + '</strong></p>';
	if (result == 'Ok') {
		if (rules.length > 0) {
			text = text + '<p>' + applied + '<strong>' + writeArray(rules) + '</strong></p></p>';
		}
		$('feedback').update(text);
		$('history').update($('history').innerHTML + '<br>' + state.exercise);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		ss_getRemaining(state, function(number) {$('progress').innerHTML = 'Steps<br> ' + number; historyKeeper.update(state);});
	}
	else if (result == 'Detour') {
		text = text + '<p><strong>' + two + '</strong></p></p>';
		if (rules.length > 0) {
			text = text + '<p>applied' + writeArray(rules) + '</strong></p></p>';
		}
		$('feedback').update(text);
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
