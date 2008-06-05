<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/genexas/css/exas.css" >
<link rel="shortcut icon" href="/genexas/css/favicon.ico" type="image/x-icon" >
<script type="text/javascript" src="http://prototype.conio.net/dist/prototype-1.4.0.js"></script>
<!--  <script type="text/javascript" src="/genexas/common/javascript/prototype-1.6.0.2.js"></script>  -->
<script type="text/javascript" src="/genexas/common/javascript/help.js"></script>
<script type="text/javascript" src="/genexas/common/javascript/services.js"></script>
<script type="text/javascript" src="<?php print Local;?>"></script>
<script type="text/javascript" src="communication.js"></script>

<script type="text/javascript" src="/genexas/common/javascript/json2007.js"></script>
<script type="text/javascript" src="/genexas/common/javascript/init.js"></script>
</head>

<h1>Exercise Assistant online</h1>
<div id="exasdiv">
<input class="menu" type="button" id="aboutButton" value="<?php print About;?>" >
<input class="menu" type="button" id="helpButton" value="<?php print Help;?>" >
<input class="menu" type="button" id="rulesButton" value="<?php print Rules;?>" >
<input class="menu" type="button" id="generateButton" value="<?php print NewExercise;?>" >
<br clear="all" >

<div class="column left">

	<h3><?php print Exercise;?></h3>
	<div id="exercise" ></div>

	<h3><?php print WorkArea;?></h3>

	<textarea id="work" rows="2" cols="40" >	
	</textarea>
	<input class="minibutton" id="submitbutton" type="button" value="<?php print Submit;?>" >	
	<input id="progressbutton" class="minibutton" type="button" value="<?php print Progress;?>" >
	<input id="nextbutton"  class="minibutton" type="button" value="<?php print Step;?>" >
	<input id="hintbutton" class="minibutton" type="button" value="<?php print Hint;?>" >
	<br class="clear">
	<input class="minibutton" type="button" id="readybutton" onclick="ready() "value="<?php print Ready;?>" >
	<input class="minibutton" type="button" id="forwardbutton" value="<?php print Forward;?>" >
	<input class="minibutton" type="button" id="undobutton" value="<?php print Back;?>" >
	<input class="minibutton" type="button" id="copybutton" value="<?php print Copy;?>" >
	<br>
	<h3><?php print History;?></h3>
	<div id="history"></div>

</div>

<div class="column right">
	
		
		<h3><?php print Feedback ?></h3>
		<input type="button" id="clearbutton" value="<?php print Clear;?>" >
		
	<div id="feedback"></div>

</div>

<div id="rules" class="helparea invisible">
<input class="helpbutton" id="closerulesButton" type="button" value="<?php print Close;?>" >
<?php rules();?>
</div>

<div id="help" class="helparea invisible">
<input class="helpbutton"  id="closehelpButton" type="button" value="<?php print Close1;?>" >
<?php help();?>
</div>

<div id="about" class="helparea invisible">
<input class="helpbutton"  id="closeaboutButton" type="button" value="<?php print Close2;?>" >
<?php about();?>

</div>

</body>
</html>