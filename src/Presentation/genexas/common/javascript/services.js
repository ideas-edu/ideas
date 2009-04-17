// The url for the services
var url = "/cgi-bin/servicegenexas.cgi";

/**
 *  Generation of a new exercise. 
  * Input: an integer
  * Output: a state object 
  * The output is passed to the callback function
  */
function ss_generate(number, callback) {
	var myAjax = new Ajax.Request(url, {   
		parameters : 'input={ "source": "genexas", "method" :"generate", "params" : ["'+ exercisekind + '", ' + number + '], "id" : ' + id + '}',	 
		onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			if (error == null) {
				result = resJSON.result;
				/*                    id          locatie   formule */
				var state = new State(result[0], result[1], presenteertekst(result[2]), result[3]);
//				var state = new State(result[0], result[1], result[2], result[3]);
				
				callback(state);
			}
			else {
				alert(error);
			}			
         },
		 onFailure: function() { 
			alert(parameters); 
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
		parameters : 'input={ "source": "genexas", "method" : "ready" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
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
		parameters : 'input={ "source": "genexas", "method" :"onefirsttext", "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
		onSuccess : function(response) {
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			//alert(response.responseText);
			if (error == null) {
				var result = resJSON['result'];
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
		parameters : 'input={ "source": "genexas", "method" : "onefirsttext" , "params" : [["'+ state.id + '", "'  + state.prefix + '", "' + exercise + '", "' + state.simpleContext + '"]], "id" : ' + id + '}',
        onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			var result = resJSON['result'];
			if (error == null) {
				var valid = result[0];
				var rule = result[1];
				var state = result[2];
				var newState = new State(state[0], state[1], presenteertekst(state[2]), state[3]);
				callback(rule, valid, newState);
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
		parameters : 'input={ "source": "genexas", "method" : "derivation" , "params" : [["'+ eastate.id + '", "'  + eastate.prefix + '", "' + exercise + '", "' + eastate.simpleContext + '"]], "id" : ' + id + '}',
        onSuccess : function(response) {	
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			if (error == null) {
				var list = resJSON["result"];
				var setOfRules = new Array();
				var counter = 0;
				while (counter < list.length) {
					var entry = list[counter];
					var appliedRule = new Rule(entry[0], entry[1], presenteertekst(entry[2]));
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
        parameters : 'input={ "source": "genexas", "method" :"stepsremaining", "params" : [["'+ eastate.id + '", "'  + eastate.prefix + '", "' + exercise + '", "' + eastate.simpleContext + '"]], "id" : ' + id + '}',
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
        parameters : 'input={ "source": "genexas", "method" : "submittext", "params" : [["'+ state.id + '", "'  + state.prefix + '", "'+ exercise + '", "' + state.simpleContext + '"], "' + newexpression + '"], "id" : ' + id + '}',
        onSuccess : function(response) {
			var resJSON = parseJSON(response.responseText);
			var error = resJSON.error;
			//alert(response.responseText);
			if (error == null) {
				var result = resJSON.result;
				//alert(result[0]);
				//alert(result[1]);
				var newState = null;
				var receivedstate = (resJSON.result)[2];
				newState = new State(receivedstate[0], receivedstate[1], presenteertekst(receivedstate[2]), receivedstate[3]);
				callback(result, newState);
			}
			else { alert(error)};
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
