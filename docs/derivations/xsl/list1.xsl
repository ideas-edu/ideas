



<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='list1' and @name='list']" >
  <xsl:call-template name="fenced"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='list1' and @name='list'][following-sibling::*/*/*]" priority="2">
<mtable>
<xsl:for-each select="following-sibling::*">
<mtr><mtd columnalign="left"><xsl:apply-templates select="."/></mtd></mtr>
</xsl:for-each>
</mtable>
</xsl:template>


<xsl:template match="om:OMS[@cd='list1' and @name='suchthat'][following-sibling::om:OMBIND/om:OMS/@name='lambda']">
<mrow>
  <mi>list</mi>
  <mo>&#x2061;</mo>
  <mrow>
    <mo>(</mo>
    <mrow>
      <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR/om:OMV"/>
      <mo>&#x2208;</mo>
      <xsl:apply-templates select="following-sibling::*[1]"/>
    </mrow>
    <mo>|<!--&#x2223;--></mo>
    <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
   <mo>)</mo>
  </mrow>
</mrow>
</xsl:template>

<xsl:template match="om:OMS[@cd='list1' and @name='map'][following-sibling::om:OMBIND/om:OMS/@name='lambda']">
<mrow>
  <mi>list</mi>
  <mo>&#x2061;</mo>
  <mrow>
   <mo>(</mo>
    <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
    <mo>|<!--&#x2223;--></mo>
    <mrow>
      <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR/om:OMV"/>
      <mo>&#x2208;</mo>
      <xsl:apply-templates select="following-sibling::*[2]"/>
    </mrow>
   <mo>)</mo>
  </mrow>
</mrow>
</xsl:template>

</xsl:stylesheet>







