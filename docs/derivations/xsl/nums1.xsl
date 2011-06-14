

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='nums1' and @name='based_integer']"  >
<msub>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  <xsl:apply-templates select="following-sibling::*[1]"/>
</msub>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='based_float']"  >
<msub>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  <xsl:apply-templates select="following-sibling::*[1]"/>
</msub>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='rational']"  >
<mfrac>
  <xsl:apply-templates select="following-sibling::*[1]"/>
  <xsl:apply-templates select="following-sibling::*[2]"/>
</mfrac>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='complex_cartesian']"  >
<mrow>
  <xsl:apply-templates select="following-sibling::*[1]"/>
<mo>+</mo>
  <mrow>
  <mi>i</mi>
  <mo><!-- IT --></mo>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  </mrow>
</mrow>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='complex_polar']"  >
<mrow>
  <xsl:apply-templates select="following-sibling::*[1]"/>
<mo>&#xD7;</mo>
  <msup>
  <mi>e</mi>
  <xsl:apply-templates select="following-sibling::*[2]"/>
  </msup>
</mrow>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='infinity']"  >
<mi>&#x221E;</mi>
</xsl:template>


<xsl:template match="om:OMS[@cd='nums1' and @name='e']"  >
<mi>e</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='i']"  >
<mi>i</mi>
</xsl:template>


<xsl:template match="om:OMS[@cd='nums1' and @name='pi']"  >
<mi>&#x3C0;</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='gamma']"  >
<mi>&#x3B3;</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='nums1' and @name='NaN']"  >
<mi>NaN</mi>
</xsl:template>

</xsl:stylesheet>

