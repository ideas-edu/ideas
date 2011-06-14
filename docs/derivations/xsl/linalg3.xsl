

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='linalg3'  and @name='matrix']" >
  <mfenced><mtable>
   <xsl:variable name="cols" select="following-sibling::om:OMA"/>
   <xsl:for-each select="following-sibling::om:OMA[1]/*[position()&gt;1]">
   <xsl:variable name="r" select="position()"/>
   <mtr>
    <xsl:for-each select="$cols">
    <mtd><xsl:apply-templates select="*[1+$r]"/></mtd>
    </xsl:for-each>
   </mtr>
   </xsl:for-each>
  </mtable></mfenced>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg3'  and @name='matrixcolumn']" >
  <mfenced><mtable>
   <xsl:for-each select="following-sibling::*">
   <mtr><mtd><xsl:apply-templates select="."/></mtd></mtr>
   </xsl:for-each>
  </mtable></mfenced>
</xsl:template>



<xsl:template match="om:OMS[@cd='linalg3'  and @name='vector']" >
   <mfenced>
   <mtable>
   <xsl:for-each select="following-sibling::*">
   <mtr><mtd><xsl:apply-templates select="."/></mtd></mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>>
</xsl:template>



</xsl:stylesheet>

