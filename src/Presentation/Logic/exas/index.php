
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>OU Exercise Assistant On-line</title>
<link rel="stylesheet" type="text/css" href="/exas/css/exas.css">
<link rel="shortcut icon" href="/exas/css/favicon.ico" type="image/x-icon">
</head>
<body>
<h1>OU Exercise Assistant On-line</h1>
<h2>Welcome on the OU Exercise Assistant on-line.</h2>
<div class="kolom">
<p>At this moment, there are two exercises available. Please enter your studentnumber or &quot;anonymous&quot;
to start the exercise.</p>
<h3>Proposition logic</h3>
<h4>Rewrite an expression into its DNF form</h4>
<p><img src="/exas/images/flag-nl.gif" alt="nl">&nbsp;Only in Dutch.</p>
<form method="post" action="/exas/proplogic/todnf/nl/login.php">
<table>
<tr cols="2">
<td>OU students: please fill in your <br>Student number</td>
</tr>
<tr>
<td><input type="text" name="naam"></td>
<td><input class="button" type="submit" value="Start exercise"></td>
</tr>
<tr cols="2">
<td>&nbsp;</td>
</tr>
<tr cols="2">
<td>Everybody else: <br>start the exercise anonymously</td>
</tr>
<tr>
<td><input type="text" name="anon" value="anonymous"></td>
<td><input class="button" type="submit" value="Start exercise"></td>
</tr>
</table>
<input type="hidden" name="soort" value="proplogic-dnf"></input>
</form>


<h3>Mathematics</h3>
<h4>Equation sets with two or three variables</h4>
<p><img src="/exas/images/flag-uk.gif" alt="nl">&nbsp;Only in English.</p>
<form method="post" action="/exas/math/equations/en/index.php">
<table>
<tr cols="2">
<td>OU students: please fill in your <br>Student number</td>
</tr>
<tr>
<td><input type="text" name="naam"></td>
<td><input class="button" type="submit" value="Start exercise"></td>
</tr>
<tr cols="2">
<td>&nbsp;</td>
</tr>
<tr cols="2">
<td>Everybody else: <br>start the exercise anonymously</td>
</tr>
<tr>
<td><input type="text" name="anon" value="anonymous"></td>
<td><input class="button" type="submit" value="Start exercise"></td>
<input type="hidden" name="soort" value="math-equations"></input>
</tr>
</table>
</form>
</div>
</body>