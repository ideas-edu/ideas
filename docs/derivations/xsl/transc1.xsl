

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='transc1' and @name='log']"  >
 <mrow>
  <msub><mi>log</mi><xsl:apply-templates select="following-sibling::*[1]"/></msub>
  <mo><!-- AF --></mo>
   <mfenced><xsl:apply-templates select="following-sibling::*[2]"/></mfenced>
 </mrow>
</xsl:template>


</xsl:stylesheet>

