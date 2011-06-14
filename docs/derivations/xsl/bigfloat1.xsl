

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='bigfloat1' and @name='bigfloat']"  >
 <mrow>
  <xsl:apply-templates select="following-sibling::*[1]"/>
  <mo>&#xD7;</mo>
  <msup>
    <xsl:apply-templates select="following-sibling::*[2]"/>
    <xsl:apply-templates select="following-sibling::*[3]"/>
  </msup>
 </mrow>
</xsl:template>


</xsl:stylesheet>

