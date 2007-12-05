<input class="menu" type="button" onclick="menuhelp('about')" value="About" >
<input class="menu" type="button" onclick="menuhelp('help')" value="Help" >
<input class="menu" type="button" onclick="genereerFormules()" value="Nieuwe Opgave" >
<br clear="all" >


<div class="kolom links">

	<h3 id="exercise">Opgave</h3>
	<div id="opgave" style="height: 100px;"></div>

	<h3 id="log">Werkveld: wijzig en controleer:</h3>
	<textarea id="werk" rows="5" cols="40" ></textarea>
	<br>
	<br clear="all">

	<input id="controleerbutton" type="button" onclick="submitAntwoord()" value="Submit" >

	<input class="minibutton" type="button" id="herstelbutton" onclick="herstel() "value="Herstel" >

	<input class="minibutton" type="button" onclick="getHint() "value="Hint" >

	<h3>Feedback</h3>
	<div id="feedback">

	</div>
	</div>

	<div class="kolom rechts">
	<h3>Afleiding</h3>
	<div id="afleiding"></div>
</div>


<div id="help" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('help') "value="Sluit" >
<?php include  'help.html';?>
</div>

<div id="about" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('about') "value="Sluit" >
<?php include  'about.html';?>

