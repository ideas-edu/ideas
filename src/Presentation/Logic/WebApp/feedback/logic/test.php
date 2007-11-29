<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/feedback/css/feedback.css" >
<link rel="shortcut icon" href="/feedback/favicon.ico" type="image/x-icon" >

<!-- Voor het url-encoderen en url-decoderen van tekst -->
<script type="text/javascript" src="/feedback/common/common.js"></script>

<!-- Om de back en forward knop te kunnen gebruiken -->
<script type="text/javascript" src="/feedback/historyframework/dhtmlHistory.js"></script>

<!-- Voor de communicatie met de server -->
<script type="text/javascript" src="/feedback/logic/javascript/clientfeedback.js"></script>

<!-- Voor helpschermen en dergelijke -->
<script type="text/javascript" src="/feedback/logic/javascript/help.js"></script>
</head>

<!-- Initialiseer zorgt voor een verse logicaregel -->
<body onload="initialiseer()">

<h1>Exercise Assistant online</h1>
<input class="button menu" type="button" onclick="menuhelp('regels', 'help')" value="Herschrijfregels" >
<input class="button menu" type="button" onclick="menuhelp('help', 'regels')" value="Help" >
<input class="button menu" type="button" onclick="genereerFormules()" value="Nieuwe Opgave" >
<br clear="all" >
<!-- Nog doen: tooltips maken met uitleg -->
<div class="form links">
	<h3>Werkveld, herschrijf en controleer:</h3>
	<textarea id="invoer" rows="10" cols="40" onchange="veranderd()">
	
	</textarea>
	<input id="controleerbutton" class="button" type="button" onclick="submitAntwoord() "value="Controleer" >
	<input id="herstelbutton" class="button onzichtbaar" type="button" onclick="herstel() "value="Herstel" >
	<input class="button" type="button" onclick="getHint() "value="Hint" >
	<input class="button" type="button" onclick="getNext() "value="Stap" >
	<input class="button" type="button" onclick="getKlaar() "value="Klaar" >

	<h3>Feedback</h3>
	<textarea id="feedback" rows="10" cols="40"></textarea>
	</div>

	<div class="form rechts">
	<h3>Invoergeschiedenis</h3>
	<textarea id="uitwerking" rows="30" cols="40"></textarea>
</div>

<div id="help" class="onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('help') "value="Sluit" >
<?php include  'help.html';?>
</div>

<div id="regels" class="onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('regels') "value="Sluit" >
<?php include  'regels.html';?>
</div>

<div id="toetsenbord" class="zichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('regels') "value="Sluit" >
<?php include  'grafisch.html';?>
</div>
</body>
</html>