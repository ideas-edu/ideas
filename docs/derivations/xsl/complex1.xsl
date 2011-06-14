
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='complex1' and @name='complex_cartesian']" >
<xsl:choose>
<xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
<xsl:variable name="r">
 <xsl:apply-templates select="following-sibling::*[1]">
  <xsl:with-param name="p" select="1"/>
 </xsl:apply-templates>
</xsl:variable>
<xsl:variable name="i">
 <xsl:apply-templates select="following-sibling::*[2]">
   <xsl:with-param name="p" select="2"/>
 </xsl:apply-templates>
</xsl:variable>
<xsl:choose>
<xsl:when test="$i = 0">
  <xsl:copy-of select="$r"/>
</xsl:when>
<xsl:when test="$r = 0 and $i = 1">
 <mi>i</mi>
</xsl:when>
<xsl:when test="$r = 0">
<mrow>
 <xsl:copy-of select="$i"/>
 <mo>&#x2062;<!-- IT --></mo>
 <mi>i</mi>
</mrow>
</xsl:when>
<xsl:when test="$i = 1">
<mrow>
<xsl:copy-of select="$r"/>
<mo>+</mo>
 <mi>i</mi>
</mrow>
</xsl:when>
<xsl:otherwise>
<mrow>
<xsl:copy-of select="$r"/>
<mo>+</mo>
<mrow>
 <xsl:copy-of select="$i"/>
 <mo>&#x2062;<!-- IT --></mo>
 <mi>i</mi>
</mrow>
</mrow>
</xsl:otherwise>
</xsl:choose>
</xsl:when>
<xsl:otherwise>
 <mi><xsl:value-of select="@name"/></mi>
</xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='complex1' and @name='complex_polar']" >
<xsl:choose>
<xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
<xsl:variable name="r">
 <xsl:apply-templates select="following-sibling::*[1]">
  <xsl:with-param name="p" select="2"/>
 </xsl:apply-templates>
</xsl:variable>
<xsl:variable name="t">
 <xsl:apply-templates select="following-sibling::*[2]">
   <xsl:with-param name="p" select="2"/>
 </xsl:apply-templates>
</xsl:variable>
<xsl:choose>
<xsl:when test="$t = 0">
  <xsl:copy-of select="$r"/>
</xsl:when>
<xsl:when test="$r = 0">
  <xsl:copy-of select="$r"/>
</xsl:when>
<xsl:when test=" $r = 1 and $t = 1">
<msup><mi>e</mi><mi>i</mi></msup>
</xsl:when><xsl:when test=" $r = 1">
<msup><mi>e</mi>
<mrow>
 <xsl:copy-of select="$t"/>
 <mo>&#x2062;<!-- IT --></mo>
 <mi>i</mi>
</mrow>
</msup>
</xsl:when>
<xsl:when test="$t = 1">
<mrow>
<xsl:copy-of select="$r"/>
<mo>&#x2062;</mo>
 <msup><mi>e</mi><mi>i</mi></msup>
</mrow>
</xsl:when>
<xsl:otherwise>
<mrow>
<xsl:copy-of select="$r"/>
<mo>&#x2062;</mo>
<msup><mi>e</mi>
<mrow>
 <xsl:copy-of select="$t"/>
 <mo>&#x2062;<!-- IT --></mo>
 <mi>i</mi>
</mrow>
</msup>
</mrow>
</xsl:otherwise>
</xsl:choose>
</xsl:when>
<xsl:otherwise>
 <mi><xsl:value-of select="@name"/></mi>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='complex1' and @name='conjugate']"  >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <mover>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <mo>&#xaf;</mo>
   </mover>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>

</xsl:stylesheet>



