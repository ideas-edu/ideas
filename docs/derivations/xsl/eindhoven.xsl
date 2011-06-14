

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:include href="cd.xsl"/>
<xsl:include href="permgp2.xsl"/>
<xsl:include href="set3.xsl"/>
<xsl:include href="integer2.xsl"/>

<xsl:template match="om:OMS[@name='isomorphic']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2245;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>


</xsl:stylesheet>
