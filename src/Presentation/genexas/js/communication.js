// The url for the services
function getURL() {
   return "cgi/service.cgi";
}

function getDifficulty() {
   var diff = 2;
   if ($('Easy').checked) {
     diff = 1;
   } else if ($('Difficult').checked) {
     diff = 3;
   }
   return diff;
}


function addToFeedback(newText) {
   var text = '';
   if (keepFeedback()) {
      text = $('feedback').innerHTML;
      if (text != '') {
         text += '<hr>';
      }
   }
   text += newText;

   $('feedback').update(text);
   $('feedback').scrollTop = $('feedback').scrollHeight;
}
