<?xml version="1.0"?>
<!-- Author: Nicola Ken Barozzi "barozzi@nicolaken.com" -->

<xsl:stylesheet version="1.0"
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 xmlns:error="http://apache.org/cocoon/error/2.0">

<xsl:template match="error:notify">
 <html>
  <head>
   <title>
    NO STS
   </title>

  </head>
<body>
<h1>No STS file</h1>
<p>This server does not have the requested STS file at present.</p>
<p>Message from tomcat:<br/>
<xsl:value-of select="error:description"/></p>
</body>
</html>

</xsl:template>
</xsl:stylesheet>
