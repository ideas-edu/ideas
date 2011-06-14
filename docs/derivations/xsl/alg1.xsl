
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='alg1' and @name='one']" >
<mn>1</mn>
</xsl:template>


<xsl:template match="om:OMS[@cd='alg1' and @name='zero']" >
<mn>0</mn>
</xsl:template>

</xsl:stylesheet>



