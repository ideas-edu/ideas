<?php
	include_once ("./common/loginhelp.php");
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
</head>
<body>

<h1>Exercise Assistant online: Overzicht</h1>
<h2>Overzicht van de studentbestanden</h2>
<h3>Propositielogica, naar DNF</h3>
<?
$dir="./proplogic/toDNF/nl/studenten";
if (is_dir($dir)) {
    if ($dh = opendir($dir)) {
    	$files = array();
        while (($file = readdir($dh)) !== false) {
        	if (substr($file, strlen($file) - 4) == '.txt') {
                array_push($files, $file);
            }         
        }
        closedir($dh);
    }
}
sort($files);
echo "<ul>\n";
foreach ($files as $file) {
	$ref = "./proplogic/toDNF/nl/studenten/".$file;
	echo "<li><a href=\"$ref\" title=\"$file\">$file</a></li>\n";
}
echo "</ul>\n";
?>
<h3>Wiskunde, vergelijkingen met x onbekenden</h3>
<?
$dir="./math/equations/en/studenten";
if (is_dir($dir)) {
    if ($dh = opendir($dir)) {
    	$files = array();
        while (($file = readdir($dh)) !== false) {
        	if (substr($file, strlen($file) - 4) == '.txt') {
                array_push($files, $file);
            }         
        }
        closedir($dh);
    }
}
sort($files);
echo "<ul>\n";
foreach ($files as $file) {
	$ref = "./proplogic/toDNF/nl/studenten/".$file;
	echo "<li><a href=\"$ref\" title=\"$file\">$file</a></li>\n";
}
echo "</ul>\n";
?>
</body>
</html>