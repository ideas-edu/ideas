
/* *
  * This file contains the initialisation
  */

document.observe("dom:loaded", init);

function init() {
   /* Feedback buttons in Working Area */
   $('backbutton').observe('click', onBackClick);
   $('readybutton').observe('click', onReadyClick);
   $('submitbutton').observe('click', onSubmitClick);
   $('hintbutton').observe('click', onHintClick);
   $('stepbutton').observe('click', onStepClick);
   $('autostepbutton').observe('click', onAutoStepClick); 
   $('workedoutbutton').observe('click', onWorkedOutClick);
   
   /* Other buttons */
   $('newexercisebutton').observe('click', onNewExerciseClick);
   $('clearbutton').observe('click', onClearClick); 
   
   /* Keypress in working area */
   addEventSimple($("work"), 'keypress', onWorkKeypress);
   
   /* Feedback choice*/
   $('lastonlychoice').observe('click', onLastOnlyClick); 
   
   $('closehelpButton').observe('click', closehelp);
   $('closeaboutButton').observe('click', closehelp);
   $('closerulesButton').observe('click', closehelp); 
   $('aboutButton').observe('click', openhelp);
   
   $('work').focus();
   onNewExercise('init');
   welcomeMessage();
}

                  
