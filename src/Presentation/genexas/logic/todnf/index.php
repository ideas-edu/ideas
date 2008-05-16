<?php
// als er een speciale user interface voor het domein is
function toetsen() {
	if (file_exists("../keys.php")) {
		include_once("../keys.php");
	}
}
include_once("../../common/framework.php");
?>