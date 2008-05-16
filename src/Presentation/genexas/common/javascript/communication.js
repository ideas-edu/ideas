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
	var exercise = $('exercise');
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"generate", "params" : [[exercisekind, 5]], "id" : id}',
         onSuccess : function(response) {
			alert(response.error);
			var resJSON = response.responseText.parseJSON();
			exercise.innerHTML = resJSON.result[2];
         }
     });
}
/**
 *  getHint gets a rule which can be applied, and puts it in the feedbackarea.
  */
function getHint()
{
	var url = "/cgi-bin/service.cgi";
	var feedbackArea = $('feedback');
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"applicable", "params" : [[exercisekind, "[]", "~(~x || ~y)", ""]], "id" : id}',
         onSuccess : function(response) {
			alert(response.error);
			var resJSON = response.responseText.parseJSON();
			feedbackArea.innerHTML = resJSON.result[2];
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
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"onefirst", "params" : [[exercisekind, "[]", "~(~x || ~y)", ""]], "id" : id}',
         onSuccess : function(response) {
			alert(response.error);
			var resJSON = response.responseText.parseJSON();
			workArea.innerHTML = resJSON.result[2];
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
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"submit", "params" : [[exercisekind, "[]", "~(~x || ~y)", ""], "~~x /\\ ~~y"], "id" : id}',
         onSuccess : function(response) {
			alert(response.error);
			var resJSON = response.responseText.parseJSON();
			feedbackArea.innerHTML = resJSON.result[2];
         }
     });
}