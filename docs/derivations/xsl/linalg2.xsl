

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='linalg2'  and @name='matrix']" >
  <mfenced><mtable>
   <xsl:apply-templates select="following-sibling::*"/>
  </mtable></mfenced>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg2'  and @name='matrixrow']" >
  <xsl:choose>
  <xsl:when test="../preceding-sibling::*[last()]/@name='matrix'">
  <mtr>
   <xsl:for-each select="following-sibling::*">
   <mtd><xsl:apply-templates select="."/></mtd>
   </xsl:for-each>
  </mtr>
  </xsl:when>
  <xsl:otherwise>
  <mfenced><mtable>
  <mtr>
   <xsl:for-each select="following-sibling::*">
   <mtd><xsl:apply-templates select="."/></mtd>
   </xsl:for-each>
  </mtr>
  </mtable></mfenced>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg2'  and @name='vector']" >
  <xsl:call-template name="fenced"/>
</xsl:template>



</xsl:stylesheet>

