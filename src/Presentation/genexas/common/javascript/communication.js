function getReadyService(callback) {
	ss_getReady(snapshot.state, callback);
}
function getDerivation() {
	ss_getDerivation(snapshot.state);
}
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
	clearFeedback();
	$('copybutton').hide();
	var task = state.exercise;
	$('exercise').update(task);
	$('work').value = task;
	$('history').update(task);
	ss_getRemaining(state, function(number) {$('progress').innerHTML = "Steps<br> " + number; historyKeeper.newSnapshot(state);});
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
	var text = "";
	if (listOfRules.length > 0) {
		rules = writeArray(listOfRules);
		text = $('feedback').innerHTML  + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rules + "</strong></p>";
	}
	else {
		text = $('feedback').innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";
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
		$('feedback').innerHTML = $('feedback').innerHTML + "<p>" + changed + "</p>";
		$('copybutton').show();
		historyKeeper.addFeedback();
	}
	else {
		ss_getNext(snapshot.get('state'), displayNext);
	}
 }
 function displayNext(rule, location, state) {
	var text = "";
	var nextExpression = (state.exercise).asciiToHtml() ;
	var expression = ((snapshot.get('state')).exercise).asciiToHtml();
	if (rule) {
		text = $('feedback').innerHTML + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rule + "</strong> rule</p><p>" + resulting + " <strong>" + nextExpression + "</strong></p><p>" + paste + "</p><p";
		var copyContent = new CopyContent(state, location);
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		historyKeeper.newSnapshot(state);
		historyKeeper.addCopy(copyContent);
	}
	else {
		text = $('feedback').innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";	
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		historyKeeper.addFeedback();
	}
	
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
		$('feedback').innerHTML = $('feedback').innerHTML + "<p>" + unchanged + "</p>";
		historyKeeper.addFeedback();
	}
	else {
		ss_getFeedback(snapshot.get('state'), workExpression, displayFeedback);
	} 
}
function displayFeedback(result, rules, state) {
	// always paste the result
	var text = $('feedback').innerHTML + "<p><strong>" + result + "</strong></p>";
	if (result == "Ok") {
		if (rules.length > 0) {
			text = text + "<p>" + applied + "<strong>" + writeArray(rules) + "</strong></p></p>";
		}
		$('feedback').update(text);
		$('history').update($('history').innerHTML + "<br>" + state.exercise);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		ss_getRemaining(state, function(number) {$('progress').innerHTML = "Steps<br> " + number; historyKeeper.update(state);});
	}
	else if (result == "Detour") {
		text = text + "<p><strong>" + two + "</strong></p></p>";
		if (rules.length > 0) {
			text = text + "<p>applied" + writeArray(rules) + "</strong></p></p>";
		}
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		ss_getRemaining(state, function(number) {$('progress').innerHTML = "Steps<br> " + number; historyKeeper.update(state);});
	}
	else {
		text = text + "<p>" + copybutton +  "</p>";
		$('feedback').update(text);
		$('feedback').scrollTop = $('feedback').scrollHeight;
		$('copybutton').show();
	}
}