// The url for the services
var url = "/cgi-bin/service.cgi";

/**
 *  Generation of a new exercise. It will be shown in the area for the exercise.
  */
function ss_generate(number, callback)
{
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"generate", "params" : ["'+ exercisekind + '", ' + number + '], "id" : ' + id + '}',	 
         onSuccess : function(response) {	
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				callback(resJSON.result);
			}
			else {
				alert(wrong);
			}			
         },
		 onFailure: function() { 
			alert(wrong); 
		} 
     });
}
function ss_getReady(expression, callback)
{
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
		asynchronous:		true,
         parameters : 'input={ "method" : "ready" , "params" : [["'+ exercisekind + '", "[]", "' + expression + '", ""]] , "id" : ' + id + '}',
         onSuccess : function(response) {		
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var solved = resJSON.result;
				callback(expression, solved);
			}
			else {
				alert(wrong);
			}	
         },
		 onFailure: function() { 
			alert(wrong); 
		} 
     });
}
/**
 *  getHint gets a rule which can be applied, and puts it in the feedbackarea.
 * TODO: we will need to control the language. For now, everything is in English
  */
function ss_getHint(expression, callback)
{
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"applicable", "params" : ["[]", ["'+ exercisekind + '", "[]", "' + expression + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var result = resJSON["result"];
				callback(expression, result);
			}
			else {
				alert(response.responseText["error"] );
				var error = response.responseText["error"];
				feedbackArea.innerHTML = "<p>" + error + " </p>";
			}
         },
		 onFailure : function() {alert(wrong);}
     });
}
/**
 *  getNext puts a possible rewriting in the workarea
  */
function ss_getNext(expression, callback)
{
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
		  parameters : 'input={ "method" :"onefirst", "params" : [["'+ exercisekind + '", "[]", "' + expression + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var rule = resJSON["result"][0];
				var number = resJSON["result"][1]
				var result = resJSON["result"][2];
				callback(expression, rule, number, result);
			}
			else { 
				feedbackArea.innerHTML = feedbackArea.innerHTML + "<p>" + sorry + "<strong>" + expressie + "</strong></p>";
			}
			
         }		 ,
		 onFailure : function() {alert(wrong);}
     });
}
/**
 *  getRenmaining puts the number of remaining steps in the feedbackarea
  */
function ss_getRemaining(expression, callback)
{
	// feedbackArea.innerHTML = feedbackArea.innerHTML  + '<p><strong>Calling service StepsRemaining:</strong><br>parameters:<br> ' + 'input={ "method" :"stepsRemaining", "params" : [["'+ exercisekind + '", "[]", "' + haskellexpression + '", ""]], "id" : ' + id + '}</p>';
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"stepsremaining", "params" : [["'+ exercisekind + '", "[]", "' + expression + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			// var resJSON = response.responseText.parseJSON();
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				callback(expression, resJSON.result);
			}
		}
     });
}
/**
 *  getFeedbackt shows feedback in the feedbackarea
  */
function ss_getFeedback(expression, newexpression, callback)
{
	//var orig = (exerciseArea.innerHTML).htmlToAscii();
	//feedbackArea.innerHTML = feedbackArea.innerHTML  + '<p><strong>Calling service Submit:</strong><br>parameters:<br> ' + 'input={ "method" : "submit", "params" : [["'+ exercisekind + '", "[]", "'+ orig + '", ""], "' + expression + '"], "id" : ' + id + '}</p>';
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" : "submit", "params" : [["'+ exercisekind + '", "[]", "'+ expression + '", ""], "' + newexpression + '"], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				//alert(response.responseText);
				callback(expression, newexpression, (resJSON.result).result, (resJSON.result).rules, (resJSON.result).state);
			}
			else { alert("huh?")};
			// var resJSON = parse(response.responseText);
			
			
			
         }
     });
}
