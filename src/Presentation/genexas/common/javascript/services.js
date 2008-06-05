// The url for the services
//var url = "/cgi-bin/service.cgi";
var url = "http://ideas/StrategyTool/bin/service.cgi";

/**
 *  Generation of a new exercise. 
  * Input: an integer
  * Output: a state object 
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
				result = resJSON.result;
				var state = new State(result[0], result[1], result[2], result[3]);
				callback(state);
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
 *  Is the exercise solved?
  * Input: a state object that should reflect the current state.
  * Output: a boolean
  */
function ss_getReady(state, callback)
{
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
		asynchronous:		true,
         parameters : 'input={ "method" : "ready" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
         onSuccess : function(response) {		
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var solved = resJSON.result;
				callback(solved);
			}
			else {
				alert(response.responseText["error"] );
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
function ss_getHint(location, state, callback)
{
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"applicable", "params" : ["[]", ["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", ""]], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var result = resJSON["result"];
				callback(result);
			}
			else {
				alert(response.responseText["error"] );
			}
         },
		 onFailure : function() {alert(wrong);}
     });
}
/**
 *  getNext puts a possible rewriting in the workarea
  */
function ss_getNext(state, callback)
{
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
		parameters : 'input={ "method" : "onefirst" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				var rule = resJSON["result"][0];
				var location = resJSON["result"][1];
				var state = resJSON["result"][2];
				var newState = new State(state[0], state[1], state[2], state[3]);
				callback(rule, location, newState);
			}
			else { 
				alert(response.responseText["error"] );
			}
			
         }		 ,
		 onFailure : function() {alert(wrong);}
     });
}
/**
 *  getRenmaining puts the number of remaining steps in the feedbackarea
  */
function ss_getRemaining(state, callback)
{
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" :"stepsremaining", "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
         onSuccess : function(response) {	
			// var resJSON = response.responseText.parseJSON();
			var resJSON = response.responseText.parseJSON();
			var error = resJSON.error;
			if (error == null) {
				callback(resJSON.result);
			}
		}
     });
}
/**
 *  getFeedbackt shows feedback in the feedbackarea
  */
function ss_getFeedback(state, newexpression, callback)
{
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request
    (url,
     {   method: 'post',
         parameters : 'input={ "method" : "submit", "params" : [["'+ state.id + '", "'  + state.prefix + '", "'+ exercise + '", "' + state.simpleContext + '"], "' + newexpression + '"], "id" : ' + id + '}',
         onSuccess : function(response) {
			var resJSON = response.responseText.parseJSON();
			alert(response.responseText);
			var error = resJSON.error;
			if (error == null) {
				//alert(response.responseText);
				var result = (resJSON.result).result;
				var rules = null;
				if ((resJSON.result).rules) {
					rules = (resJSON.result).rules;
				}
				var newState = null;
				if ((resJSON.result).state) {
					var receivedstate = (resJSON.result).state;
					newState = new State(receivedstate[0], receivedstate[1], receivedstate[2], receivedstate[3]);
				}
				callback(result, rules, newState);
			}
			else { alert("huh?")};
			// var resJSON = parse(response.responseText);
         }
     });
}
