/*
 * Javascript code which is specific for a domain and a kind of exercise.
 * Here, services can be combined.
 */
var exercisekind = "To conjunctive normal form";
var id = 421;


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


