

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<xsl:template match="om:OMS[@cd='quant1' and @name='forall']">
  <xsl:call-template name="prefix">
    <xsl:with-param name="mo"><mo>&#x2200;</mo></xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='quant1' and @name='exists']">
  <xsl:call-template name="prefix">
    <xsl:with-param name="mo"><mo>&#x2203;</mo></xsl:with-param>
  </xsl:call-template>
</xsl:template>


</xsl:stylesheet>



