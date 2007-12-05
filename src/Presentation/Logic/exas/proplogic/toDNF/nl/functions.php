<?php
// functions.php

function secure () {
  if (!($_SESSION['naam']) || ($_SESSION['naam'] == "") ) {
    Header("Location: ./docentlogin.php");
    exit();
  }
}
function login_check ($forms) {
  $error = "";
  $docent = $forms["name"];
  $password = $forms["password"];
  if (trim($docent) == "") $error .= "<li>De naam is niet ingevuld</li>";
  if (trim($password) == "") $error .= "<li>Het password is niet ingevuld</li>";
  $testfile = fopen(".geheim", "r");
  $contents = fread($testfile, filesize(".geheim"));
  fclose($testfile);
  if (!(strstr($contents, $docent )) or !(strstr($contents, $password))){
	$error = "Naam of password is niet correct.";
  }
  if (trim($error)!="") return $error;
}

function login ($forms) {
  $docent = $forms["name"];
  $password = $forms["password"];
  $testfile = fopen(".geheim", "r");
  $contents = fread($testfile, filesize(".geheim"));
  fclose($testfile);
  if ((strstr($contents, $docent )) and (strstr($contents, $password))){
	return $docent;
  }
}
?>