<?php
	session_start();
	if (!($_SESSION['studentnummer']) || ($_SESSION['studentnummer'] == "")) {
    	Header("Location: ../login.php");
    	exit();
  	}
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/feedback/css/feedback.css" >
<link rel="shortcut icon" href="/feedback/favicon.ico" type="image/x-icon" >


<script type="text/javascript" src="/feedback/common/common.js"></script>


<script type="text/javascript" src="/feedback/historyframework/dhtmlHistory.js"></script>


<script type="text/javascript" src="/feedback/logic/javascript/clientfeedback.js"></script>


<script type="text/javascript" src="/feedback/logic/javascript/help.js"></script>
</head>


<body onload="initialiseer()">

<h1>Exercise Assistant online</h1>
<input class="menu" type="button" onclick="menuhelp('about')" value="About" >
<input class="menu" type="button" onclick="menuhelp('help')" value="Help" >
<input class="menu" type="button" onclick="menuhelp('regels')" value="Herschrijfregels" >
<input class="menu" type="button" onclick="genereerFormules()" value="Nieuwe Opgave" >
<br clear="all" >


<div class="kolom links">


	<h3 id="exercise">Opgave</h3>
	<div id="opgave" ></div>

	<h3 id="log">Werkveld: wijzig en controleer:</h3>
	<textarea id="werk" rows="2" cols="40" onKeyDown="controleer(event)">	
	</textarea>

	<input id="controleerbutton" type="button" onclick="submitAntwoord() "value="Submit" >

	<input class="button" type="minibutton" id="herstelbutton" onclick="herstel() "value="Herstel" >

	<input class="minibutton onzichtbaar" type="button" onclick="getHint() "value="Hint" >

	<input class="minibutton onzichtbaar" type="button" onclick="getNext() "value="Stap" >

	<input id="klaarbutton" class="minibutton onzichtbaar" type="button" onclick="getKlaar() "value="Klaar" >

	<h3>Feedback</h3>
	<textarea id="feedback"></textarea>
	</div>

	<div class="kolom rechts">
	<h3>Afleiding</h3>
	<div id="afleiding"></div>
</div>


<div id="regels" class=" helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('regels') "value="Sluit" >
<?php include  'regels.html';?>
</div>

<div id="help" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('help') "value="Sluit" >
<?php include  'help3.html';?>
</div>

<div id="about" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('about') "value="Sluit" >
<?php include  'about.html';?>
</div>
</body>
</html>