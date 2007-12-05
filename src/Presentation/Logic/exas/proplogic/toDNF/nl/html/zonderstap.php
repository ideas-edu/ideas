<input class="menu" type="button" onclick="menuhelp('about')" value="About" >
<input class="menu" type="button" onclick="menuhelp('help')" value="Help" >
<input class="menu" type="button" onclick="menuhelp('regels')" value="Herschrijfregels" >
<input class="menu" type="button" onclick="genereerFormules()" value="Nieuwe Opgave" >
<br clear="all" >


<div class="kolom links">

	<h3 id="exercise">Opgave</h3>
	<div id="opgave" ></div>

	<h3 id="log">Werkveld: wijzig en controleer:</h3>
	<textarea id="werk" rows="2" cols="40" >	
	</textarea>
	<div id="keyboard">
	<table>
	<tr>
	<td colspan="4">Toetsenbord toetsen</td>
	</tr>
	<tr>
	<td>&nbsp;=&nbsp;</td>
	<td>(is gelijk)</td>
	<td>voor:</td>
	<td>&#8596;</td>
	</tr>
	<tr>
	<td>&nbsp;i&nbsp;</td>
	<td>(implicatie)</td>
	<td>voor:</td>
	<td>&rarr;</td>
	</tr>
	<tr>
	<td>&nbsp;a&nbsp;</td>
	<td>(and)</td>
	<td>voor:</td>
	<td>&and;</td>
	</tr>
	<tr>
	<td>&nbsp;o&nbsp;</td>
	<td>(or)</td>
	<td>voor:</td>
	<td>&or;</td>
	</tr>
	<tr>
	<td>&nbsp;-&nbsp;</td>
	<td>(minteken)</td>
	<td>voor:</td>
	<td>&#172;</td>
	</tr>
	</table>
	</div>
	<div id="toetsenbord">
	<div id="toetsenbordtitel">Muis toetsenbord</div>
	<br clear="right">
	<input id="nottoets" class="toets" value="&#172;" type="button" onclick='voegin("&#172;", "werk")'>
	<input id="ortoets" class="toets" value="&#8744;" type="button" onclick='voegin("&#8744;", "werk")' >
	<input id="andtoets" class="toets" value="&#8743;" type="button" onclick='voegin("&#8743;", "werk")' >
	<input id="implicatietoets" class="toets" value="&#8594;" type="button" onclick='voegin("&#8594;", "werk")' >
	<input id="equivalentietoets" class="toets" value="&#8596;" type="button" onclick='voegin("&#8596;", "werk")' >
	<br clear="right">
	<input id="truetoets" class="toets" value="T" type="button" onclick='voegin("T", "werk")' >
	<input id="falsetoets" class="toets" value="F" type="button" onclick='voegin("F", "werk")' >
	<input id="truetoets" class="toets" value=")" type="button" onclick='voegin(")", "werk")' >
	<input id="falsetoets" class="toets" value="(" type="button" onclick='voegin("(", "werk")' >
	<input id="spacetoets" class="toets" value=" " type="button" onclick='voegin(" ", "werk")' >
	<br clear="right">
	<input id="truetoets" class="toets" value="t" type="button" onclick='voegin("t", "werk")' >
	<input id="falsetoets" class="toets" value="s" type="button" onclick='voegin("s", "werk")' >
	<input id="truetoets" class="toets" value="r" type="button" onclick='voegin("r", "werk")' >
	<input id="falsetoets" class="toets" value="q" type="button" onclick='voegin("q", "werk")' >
	<input id="falsetoets" class="toets" value="p" type="button" onclick='voegin("p", "werk")' >
	</div>
	<br>
	<br clear="all">

	<input id="controleerbutton" type="button" onclick="submitAntwoord()" value="Submit" >

	<input class="minibutton" type="button" id="herstelbutton" onclick="herstel() "value="Herstel" >

	<input class="minibutton" type="button" onclick="getHint() "value="Hint" >

	<input id="klaarbutton" class="minibutton onzichtbaar" type="button" onclick="getKlaar() "value="Klaar" >

	<h3>Feedback</h3>
	<div id="feedback">

	</div>
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
<?php include  'helpzonderstap.html';?>
</div>

<div id="about" class="helpgebied onzichtbaar">
<input class="helpbutton" type="button" onclick="sluitmenuhelp('about') "value="Sluit" >
<?php include  'about.html';?>

