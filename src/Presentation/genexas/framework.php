<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="css/exas.css" >
<link rel="shortcut icon" href="css/favicon.ico" type="image/x-icon" >
<script type="text/javascript" src="js/prototype-1.6.0.2.js"></script> 
<script type="text/javascript" src="js/help.js"></script>
<script type="text/javascript" src="js/services.js"></script>
<script type="text/javascript" src="js/communication.js"></script>
<script type="text/javascript" src="<?php print getLanguage();?>"></script>
<script type="text/javascript">var exercisekind = <?php $kind = getKind(); echo '"'.$kind.'";'; ?>var id=421;</script>
<?php if (getStudentNumber() != "") print getStudentNumber();?>
<script type="text/javascript" src="js/init.js"></script>
<script type="text/javascript" src="js/keyboard.js"></script>	
<script type="text/javascript" src="js/en.js"></script>	
</head>

<h1>Exercise Assistant Online</h1>
<div id="exasdiv">
  <div class="topmenu">
      <input type="button" id="generateButton" value="<?php print NewExercise;?>">
      <label for="Easy">Easy</label></td><td><input type="radio" name="difficulty" value="Easy" id="Easy">
      <label for="Normal">Normal</label><input type="radio" name="difficulty" value="Normal" id="Normal" checked="checked">
      <label for="Difficult">Difficult</label><input type="radio" name="difficulty" value="Difficult" id="Difficult">
      <input type="button" onclick="window.open('docs/rules.pdf','','')" value="<?php print Rules;?>">
      <input type="button" id="aboutButton" value="<?php print About;?>">
  </div>

  <br class="clear" >

  <div class="column left">
	<h3><?php print Exercise;?></h3>
	<div id="exercise" ></div>

	<h3><?php print WorkArea;?></h3>

	<input type="text" id="work" rows="2" cols="40"/>
	<table style="margin-left: 20px;" width="500px">
		<tr>
			<td><input class="minibutton" type="button" id="undobutton" value="<?php print Back;?>"></td>
			<td></td>
			<td><input class="minibutton" type="button" id="readybutton" value="<?php print Ready;?>"></td>
			<td><input class="minibutton" style="width: 160px;" type="button" id="submitbutton" value="<?php print Submit;?>"></td>
		</tr>
		<tr>
			<td><input class="minibutton" id="hintbutton" type="button" value="<?php print Hint;?>"></td>
			<td><input class="minibutton" id="nextbutton" type="button" value="<?php print Step;?>"></td>
			<td><input class="minibutton" type="button" id="copybutton" value="<?php print Copy;?>"></td>
			<td><input class="minibutton" style="width: 160px;" id="derivationbutton" type="button" value="<?php print Derivation;?>"></td>
		</tr>
	</table>
	
	<div align="center" width="100%"><br/><?php include("include/keys.php"); ?></div>
</div>

<div class="column right">
	<h3><?php print Feedback ?></h3>
	<div id="feedback" class="clear"><?php print Welcome;?></div>
	<table><tr><td>
		&nbsp;&nbsp;<input class="feedbacklabel" type="checkbox" name="feedbackchoice" id="feedbackclearchoice" checked value="chooseclear">
		<label class="feedbacklabel" for="feedbackclearchoice"><?php print ChooseClear;?></label>

	</td><td>
		<input type="button" id="clearbutton" value="<?php print Clear;?>"  style="display: none">
	</td></tr></table>
	
       	<h3><?php print History;?></h3>
	<div id="history"></div>
	
</div>

<div id="rules" class="helparea invisible">
<input class="helpbutton" id="closerulesButton" type="button" value="<?php print Close;?>" >
<?php rules();?>
</div>

<div id="help" class="helparea invisible">
<input class="helpbutton"  id="closehelpButton" type="button" value="<?php print Close;?>" >
<?php help();?>
</div>

<div id="about" class="helparea invisible">
<input class="helpbutton"  id="closeaboutButton" type="button" value="<?php print Close;?>" >
<?php about();?>

</div>
</div>
</body>
</html>
