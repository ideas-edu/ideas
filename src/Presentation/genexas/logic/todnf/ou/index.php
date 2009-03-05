<?php
session_start();
if (!($_SESSION['oustudent']) || ($_SESSION['oustudent'] == "")) {
   Header("Location: ./login.php");
   exit();
}
global $studentid;
$studentid = $_SESSION["oustudent"];
if (!isset($COOKIE['oustudent'])) {
	setcookie('oustudent', $_SESSION["oustudent"], 60*60*24*60, '/genexas/');
}
function rules() {
	// specific rules for the kind of exercise
	if (file_exists("./../en/rules.html")) {
		include_once("./../en/rules.html");
	}
	else {
		// specific rules for the domain
		if (file_exists("./../../en/rules.html")) {
			include_once("./../../en/rules.html");
		}
		else {
			// default
			include_once("../../../common/en/rules.html");
		}
	}
}
function help() {
	if (file_exists("../en/help.html")) {
		include_once("../en/help.html");
	}
	else {
		if (file_exists("../../en/help.html")) {
			include_once("../../en/help.html");
		}
		else {
			include_once("../../../common/en/help.html");
		}
	}
}
function about() {
	if (file_exists("../en/about.html")) {
		include_once("../en/about.html");
	}
	else {
		include_once("../../../common/en/about.html");
	}
}
include_once("../../../common/en.php");
function getKind() {
	return "Proposition%20to%20DNF";
}
function getStudentNumber() {
	global $studentid;
	if ($studentid && $studentid != "") {
		$content = 'id ='.$studentid.';';
		return '<script type="text/javascript">'.$content.'</script>';
	}
	else return "";
}
function getLocal() {
	global $studentid;
	if ($studentid && ($studentid != "")) {
		$content = "$('progress').invoke('hide');";
		return '<script type="javascript">'.$content.'</script>';
	}
	else return "";
}
include_once("../../../common/framework.php");
?>