<?php
function rules() {
	include_once("common/en/rules.html");
}
function help() {
	include_once("common/en/help.html");
}
function about() {
	include_once("common/en/about.html");
}
include_once("common/en.php");

function getKind() {
	return $_GET["exercisekind"];
}

include("common/framework.php");
?>