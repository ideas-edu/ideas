

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='linalg1'  and @name='vectorproduct']" >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#xD7;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg1'  and @name='scalarproduct']" >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>.</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg1'  and @name='outerproduct']" >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2297;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg1'  and @name='transpose']" >
 <msup>
  <xsl:apply-templates select="following-sibling::*"/>
  <mi>T</mi>
 </msup>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg1'  and @name='determinant']" >
 <mrow>
  <mi>det</mi>
  <mfenced>
  <xsl:apply-templates select="following-sibling::*"/>
  </mfenced>
 </mrow>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg1'  and @name='vector_selector']" >
 <msub>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  <xsl:apply-templates select="following-sibling::*[1]"/>
 </msub>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg1'  and @name='matrix_selector']" >
 <msub>
  <xsl:apply-templates select="following-sibling::*[3]"/>
  <mfenced><xsl:apply-templates select="following-sibling::*[position()&lt;3]"/></mfenced>
 </msub>
</xsl:template>

</xsl:stylesheet>

