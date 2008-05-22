// The url for the services
var url = "/cgi-bin/service.cgi";

// This will be in local.js: local to each kind of exercise
var exercisekind = "Proposition to DNF";
var id = 421;

/**
 *  Generation of a new exercise. It will be shown in the area for the exercise.
  */
function generate()
{
 	closeallhelp();
	// The expression will be shown in the work area and in the exrecise area
	var exercise = $('exercise');
	var workarea  = $('work');
	var feedbackArea = $('feedback');
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
		asynchronous:		true,
         parameters : 'input={ "method" : "generate" , "params" : ["'+ exercisekind + '", 5] , "id" : ' + id + '}',
         onSuccess : function(response) {			
			var resJSON = response.responseText.parseJSON();
			var task = resJSON.result[2];
			exercise.innerHTML = task;
			workarea.value = task;
			feedbackArea.innerHTML = "<p>In JSON: <br>" + response.responseText + "</p>";
         },
		 onFailure: function() { 
			alert('Something went wrong...'); 
		} 
     });
}
/**
 *  getHint gets a rule which can be applied, and puts it in the feedbackarea.
 * TODO: we will need to control the language. For now, everything is in English
  */
function getHint()
{
	var url = "/cgi-bin/service.cgi";
	var feedbackArea = $('feedback');
	var expressie = ($('work')).value;
	var haskellexpressie = expressie.htmlToAscii();
	// clean feedbackarea 
	feedbackArea.innerHTML = "";

	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"applicable", "params" : ["[]", ["'+ exercisekind + '", "[]", "' + haskellexpressie + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			if (response.responseText["error"] == null) {
				var result = resJSON["result"];
				feedbackArea.innerHTML = "<p>A rule which can be applied to <strong>" + expressie + "</strong> is:<br><br><strong>" + result + "</strong></p><p>In JSON: <br>" + response.responseText + "</p>";
			}
			else {
				alert(response.responseText["error"] );
				var error = response.responseText["error"];
				feedbackArea.innerHTML = "<p>" + error + " </p>";
			}
         }
     });
}
/**
 *  getNext puts a possible rewriting in the workarea
  */
function getNext()
{
	var url = "/cgi-bin/service.cgi";
	var workArea = $('work');
	var feedbackArea = $('feedback');
	var expressie = ($('work')).value;
	var haskellexpressie = expressie.htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"onefirst", "params" : [["'+ exercisekind + '", "[]", "' + haskellexpressie + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			var resJSON = response.responseText.parseJSON();
			var regel = resJSON["result"][0];
			feedbackArea.innerHTML = "<p><p>A rule which can be applied to <strong>" + expressie + "</strong> is the:<br><br><strong>" + regel + "</strong> rule</p><p>The result of the rule applied to expression is shown in the Work Area.</p><p>In JSON: <br>" + response.responseText + "</p>";
			var resultaat = resJSON["result"][2];
			workArea.value = resultaat[2];
         }
     });
}
/**
 *  getRenmaining puts the number of remaining steps in the feedbackarea
  */
function getRemaining()
{
	var url = "/cgi-bin/service.cgi";
	var feedbackArea = $('feedback');
	var expression = ($('work')).value;
	var haskellexpression = expression.htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"stepsremaining", "params" : [["'+ exercisekind + '", "[]", "' + haskellexpression + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			var resJSON = response.responseText.parseJSON();
			var feedback = resJSON.result;
			feedbackArea.innerHTML = "<p>For the expression <strong>" + expression + "</strong>, you need, at a minimum: <br><strong>" + feedback + "</strong> steps to reach the solution.</p><p>In JSON: <br>" + response.responseText + "</p>";
         }
     });
}
/**
 *  getFeedbackt shows feedback in the feedbackarea
  */
function getFeedback()
{
	closeallhelp();
	var url = "/cgi-bin/service.cgi";
	var feedbackArea = $('feedback');
	var expression = (($('work')).value).htmlToAscii();
	var orig = (($('exercise')).innerHTML).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" : "submit", "params" : [["'+ exercisekind + '", "[]", "'+ orig + '", ""], "' + expression + '"], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			var feedback = resJSON.result;
			var summaryfeedback = feedback.result;
			feedbackArea.innerHTML = "<p>De feedback is: <br><strong>" + summaryfeedback + "</strong></p><p>In JSON: <br>" + response.responseText + "</p>";
         }
     });
}
/**
 * Uitbreiding van String, om HTML special characters om te zetten naar ascii
  */
String.prototype.htmlToAscii = function() {
	var resultstring = this.replace(/&gt;/g, '>');
	resultstring = resultstring.replace(/&lt;/g, '<');
	resultstring = resultstring.replace(/\\/g, '\\\\');
	 return resultstring;
}