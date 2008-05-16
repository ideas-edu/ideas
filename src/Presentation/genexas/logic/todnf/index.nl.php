<?php

function rules() {
	// specific rules for the kind of exercise
	if (file_exists("./nl/rules.html")) {
		include_once("./hnl/rules.html");
	}
	else {
		// specific rules for the domain
		if (file_exists("./../nl/rules.html")) {
			include_once("./../nl/rules.html");
		}
		else {
			// default
			include_once("../../common/nl/rules.html");
		}
	}
}
function help() {
	if (file_exists("./nl/help.html")) {
		include_once("./nl/help.html");
	}
	else {
		if (file_exists("./../nl/help.html")) {
			include_once("./../hnl/help.html");
		}
		else {
			include_once("../../common/nl/help.html");
		}
	}
}
function about() {
	if (file_exists("./nl/about.html")) {
		include_once("./nl/about.html");
	}
	else {
		include_once("../../common/nl/about.html");
	}
}
include_once("../../common/nl.php");
include_once("index.php");
?>