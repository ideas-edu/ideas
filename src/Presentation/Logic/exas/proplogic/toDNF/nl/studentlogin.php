<?php
function enter_check($forms) {
  $error = "";
  $exerciser = $forms["naam"];
	if (trim($exerciser) == "") $error .= "<p>Vul je studentnummer in.</p>";
	if (trim($error)!="") return $error;
}

function geefnummer($forms) {
  $studentnummer = $forms["naam"];
  return $studentnummer;
}

function checknummer($nummer) {
	$studenten = simplexml_load_file("studenten/studenten/studenten.xml");
	foreach ($studenten->student as $studentnummer) {
		if ($studentnummer->nummer == $nummer) {
			return (string) $studentnummer->url;
		}
	}
}

session_start();
if ($_POST) 
{
	$error = enter_check($_POST);
	 if (trim($error)=="") {
		$_SESSION["eastudent"] = geefnummer($_POST);
		$nummer = $_SESSION["eastudent"];
		$url = checknummer($nummer);
		$_SESSION["eaurl"] = $url;
		if ((string) $url != "") {
			Header("Location: $url"); 
			exit();
		}
		else {
			Header("Location: /exas/onbekendestudent.php"); 
		exit();
		}
	} 
	else {
		// er is iets misgegaan bij inloggen */
		Header("Location: /exas/student.php"); 
		exit();
	} 
}
else {
	Header("Location: /exas/student.php"); //
}
?>
