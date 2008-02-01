<?php
function scripts() {
	print "<script type=\"text/javascript\" src=\"javascript/gedrag.js\"></script>\n";
	print "<script type=\"text/javascript\" src=\"javascript/domein.js\"></script>\n";
	print "<script type=\"text/javascript\" src=\"javascript/local.js\"></script>\n";
}

function toetsen() {
	include_once("../keys.php");
}
include_once("../../common/html/framework.php");
?>