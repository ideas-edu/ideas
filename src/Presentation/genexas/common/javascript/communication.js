/**
* functions to use the services in a straightforward way. 
* for each service, there is a calling function and a function that will be called back when the results are available.
 */
function generateService(callback) {
	ss_generate(5, callback);
}
function getReadyService(callback) {
	ss_getReady(snapshot.state, callback);
}
function getHintService(callback) {
	ss_getHint("", snapshot.state, callback);
}
function getNextService(callback) {
	ss_getNext(snapshot.state, callback);
}
function getDerivation() {
	ss_getDerivation(snapshot.state);
}
function getRemainingService(callback) {
	ss_getRemaining(snapshot.state, callback);
}
function getFeedbackService(expression, callback) {
	ss_getFeedback(snapshot.state, expression, callback);
}
/**
 * Check whether the work area contains the last valid step
 */
 function checkWorkArea() {
	var feedbackArea = $('feedback');
	var workExpression = (($('work')).value).htmlToAscii();
	var result = false;
	if (workExpression != (((snapshot.state).exercise).htmlToAscii())) {
		newSnapshot(snapshot.exercise, feedbackArea.innerHTML, snapshot.history, workExpression, snapshot.copy, snapshot.state, snapshot.location);
		result = true;
	}
	return result;
 }
/**
 * adds an expression to the historyArea, and takes a snapshot
 */
 function addStep(state) {
	$('history').innerHTML = $('history').innerHTML + "<br>" + (state.exercise).asciiToHtml();
	newSnapshot(snapshot.exercise, $('feedback').innerHTML, $('history').innerHTML, state.exercise, new CopyContent(state, snapshot.location), state, snapshot.location);
 }
/**
 * Display functions
 * Here, we fill all appropriate areas.
  * When a specific exercise chooses not to show certain areas, those areas will be filled, but remain invisible.
 */
 /**
  * A new exercise.
  * Updates the exercise area, the work area and the history area.
  * A new snapshot is taken for the back button
  */
function displayNewExercise() {
	closeallhelp();
	clearFeedback();
	var task = (snapshot.state).exercise;
	$('exercise').update(task);
	adjustHeight($('exercise'), task, 40, 40);
	adjustRows($('work'), task, 40);
	$('work').value = task;
	$('history').update(task);
}
/**
 * Displays a text in the feedbackArea
 */
 function displayFeedback(feedback) {
	closeallhelp();
	var feedbackArea = $('feedback');
	feedbackArea.innerHTML = feedback;
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
	addFeedback(feedbackArea.innerHTML);
 }
/**
 * Displays the minimal number of steps in the progress area.
 */
function displaySteps(number) {
	var stepsArea = $('progress');
	stepsArea.innerHTML = "Steps<br> " + number;
}

