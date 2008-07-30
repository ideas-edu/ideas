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
function getKind() {
	return $_GET["exercisekind"];
}
function getLocal() {
	return "";
}
include_once("common/en.php");
include("common/framework.php");
?>