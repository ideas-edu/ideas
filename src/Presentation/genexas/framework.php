<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="css/exas.css" >
<link rel="shortcut icon" href="css/favicon.ico" type="image/x-icon" >
<script type="text/javascript" src="js/prototype-1.6.0.2.js"></script> 
<script type="text/javascript" src="js/events.js"></script>
<script type="text/javascript" src="js/help.js"></script>
<script type="text/javascript" src="js/services.js"></script>
<script type="text/javascript" src="js/communication.js"></script>
<script type="text/javascript">var exercisekind = <?php $kind = getKind(); echo '"'.$kind.'";'; ?>var id=421;</script>
<?php if (getStudentNumber() != "") print getStudentNumber();?>
<script type="text/javascript" src="js/init.js"></script>
<script type="text/javascript" src="js/keyboard.js"></script>	
</head>

<h1>Exercise Assistant Online</h1>
<div id="exasdiv">
  <div class="topmenu">
      <input type="button" id="newexercisebutton" value="New exercise">
      <label for="Easy">Easy</label></td><td><input type="radio" name="difficulty" value="Easy" id="Easy">
      <label for="Normal">Normal</label><input type="radio" name="difficulty" value="Normal" id="Normal" checked="checked">
      <label for="Difficult">Difficult</label><input type="radio" name="difficulty" value="Difficult" id="Difficult">
      <input type="button" onclick="window.open('docs/rules.pdf','','')" value="Rewrite rules">
      <input type="button" id="aboutButton" value="About">
  </div>

  <br class="clear" >

  <div class="column left">
	<h3>Exercise</h3>
	<div id="exercise" ></div>

	<h3>Working area: rewrite and submit</h3>

	<input type="text" id="work" rows="2" cols="40"/>
	<table style="margin-left: 20px;" width="500px">
		<tr>
			<td><input class="minibutton" type="button" id="backbutton" value="Back"></td>
			<td></td>
			<td><input class="minibutton" type="button" id="readybutton" value="Ready"></td>
			<td><input class="minibutton" style="width: 160px;" type="button" id="submitbutton" value="Submit"></td>
		</tr>
		<tr>
			<td><input class="minibutton" id="hintbutton" type="button" value="Hint"></td>
			<td><input class="minibutton" id="stepbutton" type="button" value="Step"></td>
			<td><input class="minibutton" type="button" id="autostepbutton" value="Auto step"></td>
			<td><input class="minibutton" style="width: 160px;" id="workedoutbutton" type="button" value="Worked-out exercise"></td>
		</tr>
	</table>
	
	<div align="center" width="100%"><br/><?php include("include/keys.php"); ?></div>
</div>

<div class="column right">
	<h3>Feedback</h3>
	<div id="feedback" class="clear"></div>
	<table><tr><td>
		&nbsp;&nbsp;<input class="feedbacklabel" type="checkbox" name="feedbackchoice" id="lastonlychoice" checked value="chooseclear">
		<label class="feedbacklabel" for="lastonlychoice">Last message only</label>

	</td><td>
		<input type="button" id="clearbutton" value="Clear area"  style="display: none">
	</td></tr></table>
	
       	<h3>Derivation</h3>
	<div id="history"></div>
	
</div>

<div id="rules" class="helparea invisible">
<input class="helpbutton" id="closerulesButton" type="button" value="Close" >
<?php rules();?>
</div>

<div id="help" class="helparea invisible">
<input class="helpbutton"  id="closehelpButton" type="button" value="Close" >
<?php help();?>
</div>

<div id="about" class="helparea invisible">
<input class="helpbutton"  id="closeaboutButton" type="button" value="Close" >
<?php about();?>

</div>
</div>
</body>
</html>
