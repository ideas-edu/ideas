<?php
// als er een speciale user interface voor het domein is
function toetsen() {
	if (file_exists("../keys.php")) {
		include_once("../keys.php");
	}
}
function getKind() {
	return "Proposition%20to%20DNF";
}
function getStudentNumber() {
	global $studentid;
	if ($studentid && $studentid != "") {
		$content = 'id = "'.$studentid.'";';
		return '<script type="javascript">'.$content.'</script>';
	}
	else return "";
}
function getLocal() {
	global $studentid;
	if ($studentid && ($studentid != "")) {
		$content = 'document.getElementById("progress").style= "display: none";';
		return '<script type="javascript">'.$content.'</script>';
	}
	else return "";
}
include_once("../../common/framework.php");
?>