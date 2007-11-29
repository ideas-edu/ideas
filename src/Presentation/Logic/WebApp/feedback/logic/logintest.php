<?php
// login.php
session_start();
include ("./testfunctions.php");
if ($_POST) 
{
	$error = login_check($_POST);
	if (trim($error)=="") 
	{
		$_SESSION["studentnummer"] = login($_POST);
		Header("Location: ./test.php"); // Redirect correct member
		exit();
	} else
	{
    	print "Error:$error";
	}
}
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/feedback/css/feedback.css">
<link rel="shortcut icon" href="/feedback/favicon.ico" type="image/x-icon">
</head>
<body>
<h1>Exercise Assistant online</h1>
<p>Om te testen:</p>
<form method="post">
Studentnummer : <input type="text" name="studentnummer"><br>
<input type="submit" value="Login">
</form>
</body>