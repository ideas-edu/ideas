<?php
	$bestand = fopen("studenten/studenten/studenten.xml", 'w');
	$bestaat = false;
	fwrite($bestand, "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<studenten>\n");
	foreach ($_POST as $key => $veld) {
			if ((substr($key, 0, 13)  == "studentnummer") && ((string) $veld != "")) {
				fwrite($bestand, "<student>\n<nummer>");
				fwrite($bestand, $veld);
				fwrite($bestand, "</nummer>\n");
				$bestaat = true;
			}
			else {
				if ($bestaat && (substr($key, 0, 3) == "url")) {
					fwrite($bestand, "<url>");
					fwrite($bestand, $veld);
					fwrite($bestand, "</url>\n</student>\n");
					$bestaat = false;
				}
			}
	}
	fwrite($bestand, "</studenten>");
	fclose($bestand);
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant</title>
<link rel="stylesheet" type="text/css" href="/exas/css/exas.css">
<link rel="shortcut icon" href="/exas/favicon.ico" type="image/x-icon">
</head>

<body >
<h1>Bewaard:  koppelingen studentnummers-variatie</h1>

<p>De koppelingen tussen studentnummers en variaties zijn bewaard.</p>
<p>Van hieruit kun je verder naar:</p>
<ul>
<li>Naar: <a href="/exas/"> Exercise Assistent Home</a><br><br></li>
<li>Naar: <a href="overzicht.php">Overzicht studenten</a><br><br></li>
<li>Naar: <a href="./student.php">DNF oefening, volledige versie</a><br><br></li>
<li>Naar: <a href="./zondernext.php">DNF oefening, zonder next step</a><br><br></li>
<li>Naar: <a href="./zonderhint.php">DNF oefening, zonder hint</a><br><br></li>
</ul>
</body>
</html>