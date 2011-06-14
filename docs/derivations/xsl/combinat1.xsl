
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='combinat1' and @name='binomial']" >
<mfenced><mtable>
 <mtr><mtd><xsl:apply-templates select="following-sibling::*[1]"/></mtd></mtr>
 <mtr><mtd><xsl:apply-templates
 select="following-sibling::*[2]"/></mtd></mtr>
</mtable>
</mfenced>
</xsl:template>

<xsl:template match="om:OMS[@cd='combinat1' and @name='multinomial']" >
<mfenced><mtable>
 <mtr><mtd><xsl:apply-templates select="following-sibling::*[1]"/></mtd></mtr>
 <mtr><mtd><xsl:apply-templates
 select="following-sibling::*[position()&gt;1]"/></mtd></mtr>
</mtable>
</mfenced>
</xsl:template>



</xsl:stylesheet>



