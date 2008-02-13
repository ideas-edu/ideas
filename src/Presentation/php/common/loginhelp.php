<?php
// functions.php

function secure () {
  if (!($_SESSION['docent']) || ($_SESSION['docent'] == "")) {
    Header("Location: ./docentlogin.php");
    exit();
  }
}
function enter_check ($forms) {
  $error = "";
  $exerciser = $forms["naam"];
  if (trim($exerciser) == "") {
	$exerciser = $forms["anon"];
	if (trim($exerciser) == "") $error .= "<li>Either a student number or &quot;anonymous&quot; should be given.</li>";
	if (trim($error)!="") return $error;
	}
}

function enter ($forms) {
  $studentnummer = $forms["naam"];
  if (! $studentnummer) {
	$studentnummer = $forms["anon"];
}
  return $studentnummer;
}

function docentlogin_check ($forms) {
  $error = "";
  $eadocent = $forms["name"];
  $password = $forms["password"];
  if (trim($eadocent) == "") $error .= "<li>De naam is niet ingevuld</li>";
  if (trim($password) == "") $error .= "<li>Het password is niet ingevuld</li>";
  $testfile = fopen(".geheim", "r");
  $contents = fread($testfile, filesize(".geheim"));
  fclose($testfile);
  if (!(strstr($contents, $eadocent )) or !(strstr($contents, $password))){
	$error = "Naam of password is niet correct.";
  }
  if (trim($error)!="") return $error;
}

function docentlogin ($forms) {
  $eadocent = $forms["name"];
  $password = $forms["password"];
  $testfile = fopen(".geheim", "r");
  $contents = fread($testfile, filesize(".geheim"));
  fclose($testfile);
  if ((strstr($contents, $eadocent )) and (strstr($contents, $password))){
	return $eadocent;
  }
}
?>