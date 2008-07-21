<?php
function rules() {
	// specific rules for the kind of exercise
	if (file_exists("./en/rules.html")) {
		include_once("./en/rules.html");
	}
	else {
		// specific rules for the domain
		if (file_exists("./../en/rules.html")) {
			include_once("./../en/rules.html");
		}
		else {
			// default
			include_once("../../common/en/rules.html");
		}
	}
}
function help() {
	if (file_exists("./en/help.html")) {
		include_once("./en/help.html");
	}
	else {
		if (file_exists("./../en/help.html")) {
			include_once("./../en/help.html");
		}
		else {
			include_once("../../common/en/help.html");
		}
	}
}
function about() {
	if (file_exists("./en/about.html")) {
		include_once("./en/about.html");
	}
	else {
		include_once("../../common/en/about.html");
	}
}
include_once("../../common/en.php");
define("Local", "ou/cookies.js");
include_once("index.php");
?>