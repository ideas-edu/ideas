<?php
function secure () {
	$url = $_SESSION["eaurl"];
	if (!$url) {
		Header("Location: /exas/student.php");
		exit();
	}
	if ($url == "") {
		Header("Location: /exas/onbekendestudent.php");
		exit();
	}
	if (! (strpos ($url, "zonderstap"))) {
		Header("Location: $url");
		exit();
	} 
}

session_start();
secure();

?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >

<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/exas/css/exas.css" >
<link rel="shortcut icon" href="/exas/favicon.ico" type="image/x-icon" >
<script type="text/javascript" src="/exas/common/javascript/dhtmlHistory.js"></script>
<script type="text/javascript" src="/exas/proplogic/todnf/javascript/gedrag.js"></script>
<script type="text/javascript" src="/exas/common/javascript/help.js"></script>
<script type="text/javascript" src="/exas/common/javascript/communicatie.js"></script>
<script type="text/javascript" src="/exas/common/javascript/utils.js"></script>
<script type="text/javascript" src="/exas/proplogic/todnf/javascript/domein.js"></script>
<script type="text/javascript" src="/exas/proplogic/todnf/nl/javascript/zonderstap.js"></script>
</head>
<body onload="startexas()">

<h1>Exercise Assistant online</h1>
<div id="exasdiv"></div>
</body>
</html>