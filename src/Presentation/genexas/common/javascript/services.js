// The url for the services
//var url = "/cgi-bin/service.cgi";
var url = "/StrategyTool/bin/service.cgi";

/**
 *  Generation of a new exercise. 
  * Input: an integer
  * Output: a state object 
  * The output is passed to the callback function
  */
function ss_generate(number, callback) {
	
	var myAjax = new Ajax.Request(url, {   
		parameters : 'input={ "method" :"generate", "params" : ["'+ exercisekind + '", ' + number + '], "id" : ' + id + '}',	 
		onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
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
   * The output is passed to the callback function
  */
function ss_getReady(state, callback) {
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {
		parameters : 'input={ "method" : "ready" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
         onSuccess : function(response) {		
			var resJSON = parseJSON(response.responseText);
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
 *  getHint gets a rule which can be applied, 
 * Input: a location and a state object that should reflect the current state.
  * Output: a set of rules (strings)
   * The output is passed to the callback function
  */
function ss_getHint(location, state, callback) {
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {  
		parameters : 'input={ "method" :"applicable", "params" : ["[]", ["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", ""]], "id" : ' + id + '}',
		onSuccess : function(response) {
			var resJSON = parseJSON(response.responseText);
			
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
  * Input: a state object that should reflect the current state.
  * Output: a rulID, a location and a state 
   * The output is passed to the callback function
  */
function ss_getNext(state, callback) {
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {
		parameters : 'input={ "method" : "onefirst" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
        onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
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
 *  getDerivation returns a complete derivation
  */
function ss_getDerivation(eastate, callback) {
	var exercise = (eastate.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {
		parameters : 'input={ "method" : "derivation" , "params" : [["'+ eastate.id + '", "'  + eastate.prefix + '", "' + exercise + '", "' + eastate.simpleContext + '"]], "id" : ' + id + '}',
        onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			if (error == null) {
				var list = resJSON["result"];
				var setOfRules = new Array();
				var counter = 0;
				while (counter < list.length) {
					var entry = list[counter];
					var appliedRule = new Rule(entry[0], entry[1], entry[2]);
					++counter;
					setOfRules.push(appliedRule);
				}
				callback(setOfRules);
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
function ss_getRemaining(eastate, callback) {
	var exercise = (eastate.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {
        parameters : 'input={ "method" :"stepsremaining", "params" : [["'+ eastate.id + '", "'  + eastate.prefix + '", "' + exercise + '", "' + eastate.simpleContext + '"]], "id" : ' + id + '}',
        onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
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
function ss_getFeedback(state, newexpression, callback) {
	var exercise = (state.exercise).htmlToAscii();
	var myAjax = new Ajax.Request(url, {
        parameters : 'input={ "method" : "submit", "params" : [["'+ state.id + '", "'  + state.prefix + '", "'+ exercise + '", "' + state.simpleContext + '"], "' + newexpression + '"], "id" : ' + id + '}',
        onSuccess : function(response) {
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			if (error == null) {
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
        }
     });
}

/**
 * Help functions
  */
function parseJSON(json){
    try{
        if(/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.test(json)){
            var j = eval('(' + json + ')');
            return j;
		}
	}catch(e){
    }
    throw new SyntaxError("parseJSON");
}