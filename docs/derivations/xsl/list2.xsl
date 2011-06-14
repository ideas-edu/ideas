

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='list2'  and @name='list_selector']" >
 <msub>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  <xsl:apply-templates select="following-sibling::*[1]"/>
 </msub>
</xsl:template>

<xsl:template match="om:OMS[@cd='list2' and @name='in']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2208;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


</xsl:stylesheet>

