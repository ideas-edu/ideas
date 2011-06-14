
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns:M="http://www.w3.org/1998/Math/MathML"
  xmlns="http://www.w3.org/1999/xhtml"
  version="1.0"
>

<xsl:import href="verb.xsl"/>
<xsl:import href="om2pmml.xsl"/>

<xsl:output encoding="iso-8859-1"/>

<xsl:template mode="attrib" match="om:OMS[@cd='attributions1' and
@name='MathML-Presentation']">
<xsl:copy-of select="following-sibling::om:OMFOREIGN[1]/*"/>
</xsl:template>

<xsl:template match="/">
<xsl:processing-instruction name="xml-stylesheet"
> type="text/xsl"  href="../omxsl/pmathml.xsl"</xsl:processing-instruction>

<html>
<head><title>Test file</title></head>
<body>
<xsl:for-each select="//om:OMOBJ">
<hr/>
<pre>
<xsl:apply-templates mode="verb" select="."/>
</pre>
<xsl:apply-templates select="."/>
<hr/>
</xsl:for-each>
</body>
</html>
</xsl:template>

</xsl:stylesheet>

