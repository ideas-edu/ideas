/****************************************************
 * Services offered by the Exercise Assistant 
 */

function generateService(caller, number, callback) {
   var params  = [exercisekind, number];
   var request = makeRequest(caller, 'generate', params);
   
   generateCallback = function(result) {
      var state = arrayToState(result);
      callback(state); };
   
   serviceCall(request, generateCallback);
}

function readyService(caller, state, callback) {
   var params   = [stateToArray(state)];
   var request  = makeRequest(caller, 'ready', params);
  
   serviceCall(request, callback);
}

function onefirsttextService(caller, state, callback) {
   var params   = [stateToArray(state), caller];
   var request  = makeRequest(caller, 'onefirsttext', params);
   
   onefirsttextCallback = function(result) {
      var valid    = result[0];
      var rule     = result[1];
      var newState = arrayToState(result[2]);
      callback(rule, valid, newState); };

   serviceCall(request, onefirsttextCallback);
}

function derivationtextService(caller, state, callback) {
   var params   = [stateToArray(state), caller];
   var request  = makeRequest(caller, 'derivationtext', params);         

   serviceCall(request, callback);
}

function stepsremainingService(caller, state, callback) {
   var params   = [stateToArray(state)];
   var request  = makeRequest(caller, 'stepsremaining', params);
   
   serviceCall(request, callback);
}

function submittextService(caller, state, newexpression, callback) {
   var params   = [stateToArray(state), newexpression, caller];
   var request  = makeRequest(caller, 'submittext', params);

   submittextCallback = function(result) {
      var newState = arrayToState(result[2]);
      callback(result, newState); };
   
   serviceCall(request, submittextCallback);
}

/****************************************************
 * Functions for making requests and service calls
 */

function serviceCall(request, callback) {
   var ajaxOptions =
      { parameters : 'input=' + request
      , onFailure  : serviceCallFailure
      , onSuccess  : function(response) {
           var resJSON = parseJSON(response.responseText);
           if (resJSON.error == null) {
              callback(resJSON.result);
           } else {
              serviceCallFailure();
           }
        }
     };
   new Ajax.Request(getURL(), ajaxOptions);
}

function serviceCallFailure() {
   alert('service call failure');
}

function makeRequest(caller, method, params) {
   var request = { "source" : "genexas"
                 , "event"  : caller
                 , "method" : method
                 , "params" : params
                 , "id"     : id
                 };
   return show(request);
}

function arrayToState(arr) {
   return new State(arr[0], arr[1], arr[2], arr[3]);
}

function stateToArray(state) {
   return [state.code, state.prefix, state.term, state.context];
}


function parseJSON(json){
   try {
      if(/^("(\\.|[^"\\\n\r])*?"|[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t])+?$/.test(json)){
         var j = eval('(' + json + ')');
         return j;
      }
   } 
   catch(e){}
   throw new SyntaxError("parseJSON");
}

/****************************************************
 * Generic functions for showing a JSON object
 */

function show(obj) {
  if (typeof obj == "string")
     return showString(obj);
   if (obj.length >= 0)
      return showArray(obj); 
   if (typeof obj == "object")
      return showObject(obj);
  return obj;
}

function showObject(obj) {
   var result="{";
   for (var i in obj) {
      if (result!="{") {result += ",";}
      result += showString(i);
      result += ": ";
      result += show(obj[i]);
   }
   result += "}"
   return result;
}

function showArray(arr) {
   var counter = 0; 
   var result  = "[";
   while (counter < arr.length) {
        if (result!="[") {result += ",";}
      result += show(arr[counter]);
      counter++;
   } 
   result+="]";
   return result;
}

function showString(txt) {
   return '"' + txt + '"';
}