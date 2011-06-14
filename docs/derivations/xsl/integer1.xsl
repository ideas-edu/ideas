
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='integer1' and @name='factorial']" >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
<mrow>
<xsl:apply-templates select="following-sibling::*">
<xsl:with-param name="p" select="5"/>
</xsl:apply-templates>
<mo>!</mo>
</mrow>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>



</xsl:stylesheet>



