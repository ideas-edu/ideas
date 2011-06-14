

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<xsl:template
 match="om:OMA/*[1][self::om:OMA/*[1][self::om:OMS[@cd='bessel and
 @name='J']]]" priority="11">
 J_{<xsl:value-of select="*[1]/*[2]"/>}
   (<xsl:value-of select="*[2]"/>)
</xsl:template>


<xsl:template match="om:OMS[@cd='bessel and @name='J']">
 J_{<xsl:value-of select="following-sibling::*[1]" priority="10"/>}
</xsl:template>



</xsl:stylesheet>



