<?php
// login.php
session_start();
include ("functions.php");
if ($_POST) 
{
	$error = login_check($_POST);
	if (trim($error)=="") 
	{
		$_SESSION["studentnummer"] = login($_POST);
		switch ($_SESSION["studentnummer"]) {
			case "838972043" :
			case "850148662" :
			case "834214983" :
			case "850219423" :
			case "838516490" :
			case "836367631" :
			case "831806630" :
			case "836403101" :
			case "838937296" :
				Header("Location: ./logic/index.php");
				break;
			case "839010224" :
			case "850044695" :
			case "850267055" :
				Header("Location: ./logic/index3.php");
				break;
			case "Josje" :
			case "Harrie" :
			case "Johan" :
			case "Sylvia" :
			case "josje" :
			case "harrie" :
			case "johan" :
			case "sylvia" :
			case "DWAbegeleider" :
			case "dwabegeleider" :
			case "anonymous" :
			case "anoniem" :
				Header("Location: ./logic/index.php");
				break;
			default :
				print "Dit studentnummer is onbekend.";
		}
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
<br><br><br>
<form method="post">
Studentnummer : <input type="text" name="student"><br>
<input type="submit" value="login">
</form>
<br><br>
<p>Om in te loggen:</p>
<p>Gebruik je studentnummer of &quot;anoniem&quot;.</p>
</body>
