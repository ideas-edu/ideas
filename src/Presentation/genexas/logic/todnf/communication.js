// This will be in local.js: local to each kind of exercise
var exercisekind = "Proposition to DNF";
var id = 421;

/**
* functions to use the services in a straightforward way. 
* for each service, there is a calling function and a function that we be called back when the results are available.
 */
 
function generate() {
	ss_generate(5, handleGenerate);
}
function handleGenerate(state) {
	closeallhelp();
	// state[0] is the exerciseID
	//state[1] is the prefix
	// state[2] is the expression
	// state[3] is the simpleContext
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	var current = $('current');
	clearFeedback();
	task = state[2];
	exerciseArea.innerHTML = task;
	workArea.value = task;
	historyArea.innerHTML = task;
	current.innerHTML = task;
	addToHistory(task, task, feedbackArea.innerHTML, task, task);
}
 
function getReady() {
	var workArea  = $('work');
	var expression = (workArea.value).htmlToAscii();
	ss_getReady(expression, handleReady);
}

function handleReady(expression, solved) {
	expression = expression.asciiToHtml()
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	var current = $('current');
	if (solved) {
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + yes + ", <strong>" + expression + "</strong> is " + ready + ".</p>";
	}
	else {
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + no + ", <strong>" + expression + "</strong> is <strong>" + not + "</strong> " + ready + ".</p>";
	}
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
}
 
function getHint() {
	var workArea  = $('work');
	var expression = (workArea.value).htmlToAscii();
	ss_getHint(expression, handleHint);
}

function handleHint(expression, listOfRules) {
	expression = expression.asciiToHtml();
	var feedbackArea = $('feedback');
	if (listOfRules.length > 0) {
		rules = writeArray(listOfRules);
		feedbackArea.innerHTML = feedbackArea.innerHTML  + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rules + "</strong></p>";
	}
	else {
		feedbackArea.innerHTML = feedbackArea.innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";
	}
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
}

function writeArray(list) {
	elements = "";
	for (var i = 0; i < list.length; ++i) {
		elements = elements + list[i] + ",<br>";
	}
	return elements;
}

function getNext() {
	var workArea  = $('work');
	var expression = (workArea.value).htmlToAscii();
	ss_getNext(expression, handleNext);
}

function handleNext(expression, rule, number, result) {
	newExpression = result[2];
	expression = expression.asciiToHtml();
	var feedbackArea = $('feedback');
	if (rule) {
		copyContent = newExpression;
		show($('copybutton'));
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + applicable + " <strong>" + expression + "</strong>:<br><br><strong>" + rule + "</strong> rule</p><p>" + newExpression.asciiToHtml() + "</p><p>" + paste + "</p><p";
	}
	else {
		feedbackArea.innerHTML = feedbackArea.innerHTML  + "<p>" + sorry + " <strong>" + expression + "</strong></p>";
	}
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
}

function getRemaining() {
	var workArea  = $('work');
	var expression = (workArea.value).htmlToAscii();
	ss_getRemaining(expression, handleRemaining);
}

function handleRemaining(expression, number) {
	expression = expression.asciiToHtml();
	var feedbackArea = $('feedback');
	feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + forexpression + " <strong>" + expression + "</strong>, " + minimum + " <br><strong>" + number + "</strong> " + steps + ".</p>";
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
}

function getFeedback() {
	var workArea  = $('work');
	var expression = (workArea.value).htmlToAscii();
	var current = $('current');
	var currentexpression = (current.innerHTML).htmlToAscii();
	ss_getFeedback(currentexpression, expression, handleFeedback);
}

function handleFeedback(expression, newexpression, result, rules, state) {
	expression = expression.asciiToHtml();
	newexpression = newexpression.asciiToHtml();
	var exerciseArea = $('exercise');
	var workArea  = $('work');
	var feedbackArea = $('feedback');
	var historyArea = $('history');
	var current = $('current');
	if (result == "Ok") {
		historyArea.innerHTML = historyArea.innerHTML + "<br>" + newexpression;
		current.innerHTML = newexpression;
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p><strong>" + result + "</strong></p></p>";
		if (rules.length > 0) {
			feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + applied + "<strong>" + writeArray(rules) + "</strong></p></p>";
		}
	}
	else if (result == "Detour") {
		historyArea.innerHTML = historyArea.innerHTML + "<br>" + expression.asciiToHtml();
		current.innerHTML = newexpression;
		var result = two;
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p><strong>" + result + "</strong></p></p>";
		if (rules.length > 0) {
			feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>applied" + writeArray(rules) + "</strong></p></p>";
		}
	}
	else if (result == "NotEquivalent") {
		show($('copybutton'));
		var result = notequivalent;
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + result + "</p><p>The previous step is under the copy button;</p>";
	} else {
		feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + result + "</p>";
	}
	addToHistory(exerciseArea.innerHTML, workArea.value, feedbackArea.innerHTML, historyArea.innerHTML, current.innerHTML);
	feedbackArea.scrollTop = feedbackArea.scrollHeight;
}