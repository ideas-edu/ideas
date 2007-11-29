<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<html>
<head>
<title>OU Feedback Engine On-line</title>
<link rel="stylesheet" type="text/css" href="http://localhost/feedback/css/feedback.css" >
<link rel="shortcut icon" href="../favicon.ico" type="image/x-icon" >

<!-- Voor het url-encoderen en url-decoderen van tekst -->
<script type="text/javascript" src="../common/common.js"></script>

<!-- Om de back en forward knop te kunnen gebruiken -->
<script type="text/javascript" src="../historyframework/dhtmlHistory.js"></script>

<!-- Voor de communicatie met de server -->
<script type="text/javascript" src="javascript/clientfeedback.js"></script>
</head>

<!-- Initialiseer zorgt voor twee verse formules -->
<body onload="initialiseer()">
<h1>Feedback engine online</h1>

<!-- Nog doen: tooltips maken met uitleg -->

<div class="form rechts">
<h3>Invoergeschiedenis</h3>
<textarea id="uitwerking" rows="10" cols="40">De uitwerking komt hier terecht.</textarea>

<h3>Voortgang</h3>
<textarea id="voortgang" rows="5" cols="40">De voortgang komt hier terecht.</textarea>

<h3>Logging</h3>
<textarea id="logging" rows="10" cols="40">Hier komt de logging te staan</textarea>
</div>

<div class="form links">
<h3>Werkveld, herschrijf en controleer:
<input style="display: inline; float: right" type="button" onclick="submitAntwoord() "value="Controleer" ></h3>
<textarea id="invoer" rows="10" cols="40" onkeydown="enter()" >
Hier komen de formules
</textarea>

<h3>Feedback</h3>
<textarea id="feedback" rows="10" cols="40">Hier komt de feedback te staan</textarea>
</div>


<br clear="all" >
<h2>Gebruiksaanwijzing</h2>
<p>Links boven zie je formules. Die kun je herschrijven.</p>
<p>Als je op &quot;Voer in&quot;  klikt. krijg je in het vak eronder de feedback te zien.</p>

</body>
</html>