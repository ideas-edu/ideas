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
function getLocal() {
	return "";
}
include_once("../../common/framework.php");
?>