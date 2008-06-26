/*
 * Javascript code which is specific for a domain and a kind of exercise.
 * Here, services can be combined.
 */
var exercisekind = "To conjunctive normal form";
var id = 421;

/**
 * Generate a new exercise
 */
 function start() {
	generateService(newExercise);
}
function newExercise(state) {
	var task = state.exercise;
	newSnapshot(task, "", task, task, new CopyContent(state, ""), state, "");
	displayNewExercise();
	getRemainingService(displaySteps);
}
/**
 * React on the ready button
 */
function getReady() {
	getReadyService(handleSolved);
}
function handleSolved(solved) {
	var expression = (snapshot.state).exercise;
	var text = "";
	if (solved) {
		text = $('feedback').innerHTML + "<p>" + yes + ", <strong>" + expression + "</strong> is " + ready + ".</p>";
	}
	else {
		text = $('feedback') + "<p>" + no + ", <strong>" + expression + "</strong> is <strong>" + not + "</strong> " + ready + ".</p>";
	}
	displayFeedback(text);
}
/**
 * React on the hint button
 */
function getHint() {
	getHintService(handleHint);
}
function handleHint(listOfRules) {
	var expression = (snapshot.state).exercise;
	var text = "";
	if (listOfRules.length > 0) {
		rules = writeArray(listOfRules);
		text = $('feedback').innerHTML  + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rules + "</strong></p>";
	}
	else {
		$('feedback').innerHTML = feedbackArea.innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";
	}
	displayFeedback(text);
}
/**
 * Get the remaining number of steps
 */
 function getRemaining() {
	// Does the work area contain the last valid step?
	if (checkWorkArea()) {
		newSnapshot(snapshot.exercise, $('feedback').innerHTML, snapshot.history, $F('work'), snapshot.copy, snapshot.state, snapshot.location);
		$('feedback').innerHTML = $('feedback').innerHTML + "<p>" + changed + "</p>";
	}
	else {
		getRemainingService(displaySteps);
	}
}
/**
 * React to the next button
 */
function getNext() {
	// Does the work area contain the last valid step?
	if (checkWorkArea()) {
		$('feedback').innerHTML = $('feedback').innerHTML + "<p>" + changed + "</p>";
		newSnapshot(snapshot.exercise, $('feedback').innerHTML, snapshot.history, $F('work'), snapshot.copy, snapshot.state, snapshot.location);
	}
	else {
		getNextService(handleNext);
	}
 }
 function handleNext(rule, location, state) {
	var text = "";
	var nextExpression = (state.exercise).asciiToHtml() ;
	var expression = ((snapshot.state).exercise).asciiToHtml();
	
	if (rule) {
		text = $('feedback').innerHTML + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rule + "</strong> rule</p><p>" + resulting + " <strong>" + nextExpression + "</strong></p><p>" + paste + "</p><p";
		var copyContent = new CopyContent(state, location);
		newSnapshot(snapshot.exercise, text, snapshot.history, ($('work')).value.asciiToHtml(), copyContent, snapshot.state, snapshot.location);
	}
	else {
		text = $('feedback').innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";	
	}
	displayFeedback(text);
}
/**
 * React to the submit button
 */
 function getFeedback() {
	var workExpression = (($('work')).value).htmlToAscii();
	// Does the work area contain the last valid step?
	if (! checkWorkArea()) {
		$('feedback').innerHTML = $('feedback').innerHTML + "<p>" + unchanged + "</p>";
		newSnapshot(snapshot.exercise, $('feedback').innerHTML, snapshot.history, $F('work'), snapshot.copy, snapshot.state, snapshot.location);
	}
	else {
		getFeedbackService(workExpression, handleFeedback);
	}
}
function handleFeedback(result, rules, state) {
	// always paste the result
	var text = $('feedback').innerHTML + "<p><strong>" + result + "</strong></p>";
	if (result == "Ok") {
		if (rules.length > 0) {
			text = text + "<p>" + applied + "<strong>" + writeArray(rules) + "</strong></p></p>";
		}
		getRemainingService(displaySteps);
		// add a step to the historywindow and take a snapshot
		addStep(state);
	}
	else if (result == "Detour") {
		text = $('feedback').innerHTML + "<p><strong>" + two + "</strong></p></p>";
		if (rules.length > 0) {
			text = $('feedback').innerHTML + "<p>applied" + writeArray(rules) + "</strong></p></p>";
		}
		getRemainingService(displaySteps);
		// add a step to the historywindow and take a snapshot
		addStep(state);
	}
	else {
		text = $('feedback').innerHTML + "<p>" + copybutton +  "</p>";
	}
	displayFeedback(text);
}