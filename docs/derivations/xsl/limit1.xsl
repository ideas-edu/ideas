
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='limit1' and @name='limit'][following-sibling::*[3][self::om:OMBIND]/om:OMS[@name='lambda']]" >
<mrow>
<munder>
<mi>limit</mi>
<mrow>
 <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR"/>
 <mo>&#x2192;</mo>
 <xsl:choose>
 <xsl:when test="following-sibling::*[2][self::om:OMS[@name='above']]">
 <msup>
 <xsl:apply-templates select="following-sibling::*[1]"/>
 <mo>+</mo>
 </msup>
 </xsl:when>
 <xsl:when test="following-sibling::*[2][self::om:OMS[@name='below']]">
 <msub>
 <xsl:apply-templates select="following-sibling::*[1]"/>
 <mo>-</mo>
 </msub>
 </xsl:when>
 <xsl:otherwise>
 <xsl:apply-templates select="following-sibling::*[1]"/>
 </xsl:otherwise>
 </xsl:choose>
</mrow>
</munder>
 <mspace width="0.1em"/>
 <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
</mrow>

</xsl:template>



</xsl:stylesheet>



