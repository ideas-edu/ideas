<?php
session_start();
if (!($_SESSION['oustudent']) || ($_SESSION['oustudent'] == "")) {
   Header("Location: login.php");
   exit();
}
global $studentid;
$studentid = $_SESSION["oustudent"];
if (!isset($COOKIE['oustudent'])) {
	setcookie('oustudent', $_SESSION["oustudent"], 60*60*24*60, '/genexas/');
}
function rules() {
	// specific rules for the kind of exercise
	if (file_exists("include/rules.html")) {
		include_once("include/rules.html");
	}
}
function help() {
	if (file_exists("include/help.html")) {
		include_once("include/help.html");
	}
}
function about() {
	if (file_exists("include/about.html")) {
		include_once("include/about.html");
	}
}
// include_once("include/en.php");
function getKind() {
	return "logic.dnf-unicode";
}
function getStudentNumber() {
	global $studentid;
	if ($studentid && is_numeric($studentid)) {
		$content = 'id = '.$studentid.';';
		return '<script type="text/javascript">'.$content.'</script>';
	}
	else return "";
}
function getLocal() {
	global $studentid;
	if ($studentid && ($studentid != "")) {
	   return '<script type="javascript">'.$content.'</script>';
	}
	else return "";
}
include_once("framework.php");
?>
