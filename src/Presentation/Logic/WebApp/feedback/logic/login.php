<?php
// login.php
session_start();
include ("functions.php");
if ($_POST) 
{
	$error = login_check($_POST);
	if (trim($error)=="") 
	{
		$_SESSION["naam"] = login($_POST);
		Header("Location: ./overzicht.php"); // Redirect correct member
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
<title>OU Feedback Engine On-line</title>
<link rel="stylesheet" type="text/css" href="/feedback/css/feedback.css">
<link rel="shortcut icon" href="/feedback/favicon.ico" type="image/x-icon">
</head>
<body>
<h1></h1>
<form method="post">
<table>
<tr>
<td>Naam : </td>
<td><input type="text" name="name"></td>
</tr>
<tr>
<td>Password : </td>
<td><input type="password" name="password"></td>
</tr>
<tr>
<td></td>
<td><input class="button" type="submit" value="login"></td>
</tr>
</table>

</form>
</body>