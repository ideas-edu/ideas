
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='piece1' and @name='piecewise']" >
<mrow>
<mo>{</mo>
<mtable>
<xsl:apply-templates select="following-sibling::*"/>
</mtable>
</mrow>
</xsl:template>


<xsl:template match="om:OMS[@cd='piece1' and @name='piece']" >
<mtr columnalign="left">
<mtd><xsl:apply-templates select="following-sibling::*[1]"/></mtd>
<mtd><xsl:apply-templates select="following-sibling::*[2]"/></mtd>
</mtr>
</xsl:template>


<xsl:template match="om:OMS[@cd='piece1' and @name='otherwise']" >
<mtr>
<mtd><xsl:apply-templates select="following-sibling::*[1]"/></mtd>
<mtd><mtext>otherwise</mtext></mtd>
</mtr>
</xsl:template>



</xsl:stylesheet>



