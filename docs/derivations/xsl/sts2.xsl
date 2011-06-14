

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<xsl:template match="om:OMS[@cd='sts2' and @name='vector_n']"  >
 <msup>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  <xsl:apply-templates select="following-sibling::*[1]"/>
 </msup>
</xsl:template>


</xsl:stylesheet>



