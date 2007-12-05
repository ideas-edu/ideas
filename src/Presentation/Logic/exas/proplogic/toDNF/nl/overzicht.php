<?php
  // overzicht.php
  include("functions.php");
  session_start();
  secure();
  
  function schrijfVariatie($url) {
	if ($url == "/exas/proplogic/todnf/nl/student.php") {
		echo "<option value=\"/exas/proplogic/todnf/nl/student.php\" selected=\"selected\">Volledige versie</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderstap.php\">Zonder Stap</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderhint.php\">Zonder Hint</option>";
		return;
	}
	if ($url == "/exas/proplogic/todnf/nl/zonderstap.php") {
		echo "<option value=\"/exas/proplogic/todnf/nl/student.php\">Volledige versie</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderstap.php\" selected=\"selected\">Zonder Stap</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderhint.php\">Zonder Hint</option>";
		return;
	}
	if ($url == "/exas/proplogic/todnf/nl/zonderhint.php") {
		echo "<option value=\"/exas/proplogic/todnf/nl/student.php\">Volledige versie</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderstap.php\">Zonder Stap</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderhint.php\"  selected=\"selected\">Zonder Hint</option>";
		return;
	}
	else {
		echo "<option value=\"/exas/proplogic/todnf/nl/student.php\" selected=\"selected\">Volledige versie</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderstap.php\">Zonder Stap</option>";
		echo "<option value=\"/exas/proplogic/todnf/nl/zonderhint.php\">Zonder Hint</option>";
		return;
	}
  }
  

  function schrijfKoppelingen() {
	$studenten = simplexml_load_file("studenten/studenten/studenten.xml");
	echo "<form id=\"variatie\" action = \"verwerk.php\" method=\"post\">\n";
	$teller = 0;
	foreach ($studenten->student as $studentdata) {
		$nummer = $studentdata->nummer;
		$url = (string) $studentdata->url;
		$naam = "studentnummer" . $teller;
		$referentie = "url" . $teller;
		echo "<input type=\"text\" name=\"$naam\" value=\"$nummer\" size=\"20\"></input>\n";
		echo "<select size=\"1\" name=\"$referentie\">";
		schrijfVariatie((string) $url);
		echo "</select>";
		echo "<br>";
		++$teller;
	}
	if ($teller == 0) {
		echo "<input type=\"text\" name=\"studentnummer0\" value=\"studentnummer\" size=\"20\"></input>";
		echo "<select size=\"1\" name=\"url0\">";
		schrijfVariatie();
		echo "</select><br>";
		++$teller;
	}
	echo "<input type=\"button\" value=\"Voeg toe\" onclick=\"voegtoe($teller)\" id=\"nieuw\"></input>\n";
	echo "<input type=\"submit\" value=\"OK\"></input><br>\n";
	echo "</form>";
  }
  
  function schrijfStudenten() {
	$dir="./studenten";
	if (is_dir($dir)) {
		if ($dh = opendir($dir)) {
			$files = array();
			while (($file = readdir($dh)) !== false) {
				if (substr($file, strlen($file) - 4) == '.txt') {
					array_push($files, $file);
				}         
			}
			closedir($dh);
		}
	}
	sort($files);
	echo "<ul>\n";
	foreach ($files as $file) {
		$ref = "./studenten/".$file;
		echo "<li><a href=\"$ref\" title=\"$file\">$file</a></li>\n";
	}
	echo "</ul>\n";}
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant Overzicht studenten</title>
<link rel="stylesheet" type="text/css" href="/exas/css/exas.css">
<script type="text/javascript" src="javascript/overzicht.js"></script>
<script type="text/javascript" src="/exas/common/javascript/utils.js"></script>
<script type="text/javascript" src="/exas/common/javascript/communicatie.js"></script>
<link rel="shortcut icon" href="/exas/favicon.ico" type="image/x-icon">
</head>

<body >
<h1>Exercise Assistant Overzicht studenten</h1>
<h2>Studenten aan versies van het tool koppelen</h2>
<h3>Gebruiksaanwijzing</h3>
<h4>Weghalen</h4>
<p>Wanneer je een studentnummer weg wilt halen, is het voldoende om het nummer te deleten en op <b>OK</b> te drukken.</p>
<h4>Toevoegen</h4>
<p>Je kunt een studentnummer toevoegen door op <b>Voeg toe</b> te klikken.<br>
Er verschijnt dan een nieuwe koppeling waarin je het studentnummer kunt invullen,
en kunt kiezen welke versie je aan de student toewijst.</p>
<p>Je kunt doorgaan met nieuwe nummers toevoegen tot je klaar bent.</p>
<p>Vervolgens klik je op <b>OK</b>.</p>
<h4>Veranderen</h4>
<p>De versie veranderen voor een student met een van de weergegeven studentnummers
doe je door een andere versie te kiezen, en op <b>OK</b> te klikken.</p>
<br>
<?php
	schrijfKoppelingen();
?>

<h2>Overzicht van de studentbestanden</h2>
<?
	schrijfStudenten();
?>
</body>
</html>