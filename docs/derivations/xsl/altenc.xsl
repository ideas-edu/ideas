<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  version="1.0"
>


<xsl:template mode="attrib" match="om:OMS[@cd='altenc' and
@name='MathML-Presentation'][following-sibling::om:OMFOREIGN]">
<xsl:choose>
<xsl:when test="local-name(following-sibling::om:OMFOREIGN[1]/*[1])='math'">
<xsl:copy-of select="following-sibling::om:OMFOREIGN[1]/*/*"/>
</xsl:when>
<xsl:otherwise>
<xsl:copy-of select="following-sibling::om:OMFOREIGN[1]/*"/>
</xsl:otherwise>
</xsl:choose>
</xsl:template>


</xsl:stylesheet>
