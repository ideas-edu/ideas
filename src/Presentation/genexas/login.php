
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
   "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
<title>Exercise Assistant Online</title>
<link rel="stylesheet" type="text/css" href="/exas/css/exas.css">
<link rel="shortcut icon" href="/exas/css/favicon.ico" type="image/x-icon">
</head>
<body>
<h1>Exercise Assistant Online</h1>
<h2>Welcome to the Exercise Assistant.</h2>
<h3>Login</h3>
<form method="post" action="handlelogin.php">
<table>
<tr>
<td>Please enter your student number:</td>
</tr>
<tr>
<td><input type="text" name="naam"></td>
<td><input class="button" type="submit" value="Start"></td>
</tr>
<tr cols="2">
<td>&nbsp;</td>
</tr>
</table>
</form>
<h3>Information about the tool</h3>
<p>This tool lets you practice bringing propositions into disjunctive 
normal form. The tool lets you submit intermediate steps, and provides
feedback on how to continue. It is advisable to make small steps since
	this will allow the system to provide you with more feedback.</p>
<p><i>Associativity</i> of operators is implicit. For example, the
system will not distinguish between 
	<font color = "#0000A0">(p &#8743; q) &#8743; r</font> and 
<font color = "#0000A0">p &#8743; (q &#8743; r)</font>. In this case there is no need to write the
parentheses.
On the other hand, <i>commutativity</i> of operators is not dealt with
automatically. You are allowed to rewrite 
	<font color = "#0000A0">p &#8743; q</font> into 
	<font color = "#0000A0">q &#8743; p</font>,
but please do so in a separate step.
</p>
<p>
When entering formulas, take into account that parentheses have to be used
when mixing different operators. The proposition 
	<font color = "#0000A0">(p &#8743; q) &#8744; r</font> is
accepted, but dropping the parentheses would result in a syntax error.
</p>
<p>
The tool uses unicode symbols for presenting the logical operators. For this
to work, a unicode font has to be available to the web-browser. In particular, when using
Internet Explorer, make sure that the webpages are displayed in a font that 
supports unicode. Go to the "Internet Options" menu-item (under "Extra"),
followed by "Fonts" (in the "General" tab). Here you can select the font to be used 
for latin-based texts. Selecting "Lucida Sans Unicode" as the font for the
webpage will do. If support of unicode is lacking, a rectangular box will
be shown instead of the operator.
</p>
</body>
