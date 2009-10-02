<?php
session_start();
	global $studentid;
	$_SESSION["oustudent"] = $_POST['naam'];
	setcookie('oustudent', $_SESSION["oustudent"], 60*60*24*60, '/genexas/');
	$studentid = $_COOKIE['oustudent'];
	Header("Location: ./index.php");
	exit();
?>