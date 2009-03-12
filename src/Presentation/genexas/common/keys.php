<!--
<div id="inputhulp">
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
-->

<br/> <!-- clear="right"> -->
	<table  id="toetsenbord">
	<tr> 
	<td id="toetsenbordtitel"  colspan="5" align="center">Keyboard</td>
	</tr>
	<tr>
	<td><input id="nottoets" class="toets" value="&#172;" type="button" onclick='voegin("&#172;", "work")'></td>
	<td><input id="ortoets" class="toets" value="&#8744;" type="button" onclick='voegin("&#8744;", "work")' ></td>
	<td><input id="andtoets" class="toets" value="&#8743;" type="button" onclick='voegin("&#8743;", "work")' ></td>
	<td><input id="implicatietoets" class="toets" value="&#8594;" type="button" onclick='voegin("&#8594;", "work")' ></td>
	<td><input id="equivalentietoets" class="toets" value="&#8596;" type="button" onclick='voegin("&#8596;", "work")' ></td>
	</tr>
	<tr>
	<td><input id="truetoets" class="toets" value="T" type="button" onclick='voegin("T", "work")' ></td>
	<td><input id="falsetoets" class="toets" value="F" type="button" onclick='voegin("F", "work")' ></td>
	<td><input id="truetoets" class="toets" value=")" type="button" onclick='voegin(")", "work")' ></td>
	<td><input id="falsetoets" class="toets" value="(" type="button" onclick='voegin("(", "work")' ></td>
	<td><input id="spacetoets" class="toets" value=" " type="button" onclick='voegin(" ", "work")' ></td>
	</tr>
	<tr>
	<td><input id="truetoets" class="toets" value="t" type="button" onclick='voegin("t", "work")' ></td>
	<td><input id="falsetoets" class="toets" value="s" type="button" onclick='voegin("s", "work")' ></td>
	<td><input id="truetoets" class="toets" value="r" type="button" onclick='voegin("r", "work")' ></td>
	<td><input id="falsetoets" class="toets" value="q" type="button" onclick='voegin("q", "work")' ></td>
	<td><input id="falsetoets" class="toets" value="p" type="button" onclick='voegin("p", "work")' ></td>
	</tr></table>
<!-- </div> -->
