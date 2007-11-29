<?php
  index.php
  include("functions.php");
  session_start();
  secure();
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/feedback/css/feedback.css">
<link rel="shortcut icon" href="/feedback/favicon.ico" type="image/x-icon">
</head>

<body >
<h1>Exercise Assistant online</h1>
<h2>Kies een van de Exercise Assistants</h2>
<h3>&#8594; <a href="/feedback/logic/">Logische vergelijkingen -Hints- -Next- </a></h3>
<p class="inspring">f1 &#8596; f2 &#8660; (f1 &#8743; f2) &#8744; ((&#8764; f1) &#8743; (&#8764; f2))</p>
<p class="inspring">
<input class="linkbutton" type="button" onclick="window.location.href='logic/index.php'" value="Hints" >
<input class="linkbutton" type="button" onclick="window.location.href='logic/index.php'" value="Next" >
Met Hints knop en Next knop</p>
<br>

<h3>&#8594; <a href="/feedback/logic/index2.php">Logische vergelijkingen -Hints- </a></h3>
<p class="inspring">f1 &#8596; f2 &#8660; (f1 &#8743; f2) &#8744; ((&#8764; f1) &#8743; (&#8764; f2))</p>
<p class="inspring">
<input class="linkbutton" type="button" onclick="window.location.href='logic/index2.php'" value="Hints" >
Met Hints knop, zonder Next knop</p>
<br>


<h3>&#8594; <a href="/feedback/logic/index3.php">Logische vergelijkingen</a></h3>
<p class="inspring">f1 &#8596; f2 &#8660; (f1 &#8743; f2) &#8744; ((&#8764; f1) &#8743; (&#8764; f2))</p>
<p class="inspring">Zonder Hints en Next knop</p>
<br>

<h3>&#8594; <a href="/feedback/math/">Wiskundige formules</a></h3>
<p class="inspring">a<sup>2</sup> + b<sup>2</sup> = c<sup>2</sup></p>
</body>
</html>