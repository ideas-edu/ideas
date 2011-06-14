

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:template match="om:OMS[@cd='setname1'  and @name='C']" >
  <mi mathvariant="double-struck">C</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname1'  and @name='N']" >
  <mi mathvariant="double-struck">N</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname1'  and @name='P']" >
  <mi mathvariant="double-struck">P</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname1'  and @name='Q']" >
  <mi mathvariant="double-struck">Q</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname1'  and @name='R']" >
  <mi mathvariant="double-struck">R</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname1'  and @name='Z']" >
  <mi mathvariant="double-struck">Z</mi>
</xsl:template>




</xsl:stylesheet>



