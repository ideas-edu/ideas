
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='arith2' and @name='inverse']" >
<xsl:call-template name="msup">
 <xsl:with-param name="script">
  <mn>-1</mn>
  </xsl:with-param>
</xsl:call-template>
</xsl:template>

<!-- times added to arith1 -->
<!-- arg works by default -->

</xsl:stylesheet>



