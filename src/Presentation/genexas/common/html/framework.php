<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/genexas/css/exas.css" >
<link rel="shortcut icon" href="/genexas/favicon.ico" type="image/x-icon" >
<script type="text/javascript" src="/genexas/common/javascript/utils.js"></script>
<script type="text/javascript" src="/genexas/common/javascript/dhtmlHistory.js"></script>
<script type="text/javascript" src="/genexas/common/javascript/help.js"></script>
<?php scripts(); ?>
</head>

<h1>Exercise Assistant online</h1>
<div id="exasdiv">
<input class="menu" type="button" onclick="menuhelp('about')" value="<?php print About;?>" >
<input class="menu" type="button" onclick="menuhelp('help')" value="<?php print Help;?>" >
<input class="menu" type="button" onclick="menuhelp('regels')" value="<?php print Rules;?>" >
<input class="menu" type="button" onclick="genereerFormules()" value="<?php print NewExercise;?>" >
<br clear="all" >


<div class="kolom links">

	<h3 id="exercise"><?php print Exercise;?></h3>
	<div id="opgave" ></div>

	<h3 id="log"><?php print WorkArea;?></h3>

	<br>
	<textarea id="werk" rows="2" cols="40" >	
	</textarea>
	<input id="controleerbutton" type="button" onclick="submitAntwoord()" value="<?php print Submit;?>" >
	<input class="minibutton" type="button" id="herstelbutton" onclick="herstel() "value="<?php print Undo;?>" >
	<input class="minibutton" type="button" onclick="getNext() "value="<?php print Step;?>" >
	<input class="minibutton" type="button" onclick="getHint() "value="<?php print Hint;?>" >
	<input id="klaarbutton" class="minibutton onzichtbaar" type="button" onclick="getKlaar() "value="<?php print Ready;?>" >
	<div id="plaatsVoorToetsen"></div>
	<br>

</div>

<div class="kolom rechts">
	<h3>Afleiding</h3>
	<div id="afleiding"></div>
		<h3>Feedback</h3>
	<div id="feedback">

</div>

<div id="regels" class=" helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('regels') "value="Sluit" >
<?php include  'regels.html';?>
</div>

<div id="help" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('help') "value="Sluit" >
<?php include  'help.html';?>
</div>

<div id="about" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('about') "value="Sluit" >
<?php include  'about.html';?>

</div>
</body>
</html>