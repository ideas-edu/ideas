<?php
include ("../../../common/loginhelp.php");
/*
  * check het bestand studenten.xml om te zien welke variant gebruikt moet worden
  */
function checknummer($nummer) {
	$studenten = simplexml_load_file("studenten/studenten/studenten.xml");
	foreach ($studenten->student as $studentnummer) {
		if ($studentnummer->nummer == $nummer) {
			return $studentnummer->url;
		}
	}
}

session_start();
if ($_POST) 
{
	$error = enter_check($_POST);
	if (trim($error)=="") {
		$_SESSION["eastudent"] = enter($_POST);
		$nummer = $_SESSION["eastudent"];
		if ((string) $nummer == "anonymous") {
			Header("Location: ./index.php");
			exit();
		}
		else {
			$url = checknummer($nummer);
			if ($url == "") {
				// de gebruiker heeft een  studentnummer opgegeven dat niet aan de proef meedoet
				// we verwijzen naar de volledige versie
				Header("Location: ./index.php");
				exit();
			}
			else {
				Header("Location: " . $url);
				exit();
			}
		}
	} 
	else {
		// er is iets misgegaan bij inloggen
		Header("Location: /exas/index.php"); 
		exit();
	}
}
else {
	Header("Location: /exas/index.php"); //
}
?>