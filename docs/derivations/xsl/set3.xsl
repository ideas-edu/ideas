

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>


<xsl:template match="om:OMS[@cd='set3' and @name='big_intersect']"  >
  <xsl:call-template name="msub">
    <xsl:with-param name="base"><mo>&#x22c2;</mo></xsl:with-param>
    <xsl:with-param name="script">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='set3' and @name='powerset']"  >
  <xsl:call-template name="prefix">
   <xsl:with-param name="mo">
  <mi mathvariant="script">P</mi>
</xsl:with-param>
 </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMS[@cd='set3' and @name='map_with_target'][following-sibling::om:OMBIND/om:OMS/@name='lambda']">"  >
<mrow>
<mo>{</mo>
<mrow>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/*[3]"/>
<mo>&#x2208;</mo>
<xsl:apply-templates select="following-sibling::*[3]"/>
</mrow>
<mo>|</mo>
<mrow>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/om:OMBVAR/om:OMV"/>
<mo>&#x2208;</mo>
<xsl:apply-templates select="following-sibling::*[2]"/>
</mrow>
<mo>}</mo>
</mrow>
</xsl:template>


<xsl:template match="om:OMS[@cd='set3' and @name='map_with_condition'][following-sibling::om:OMBIND[2]]">
<mrow>
<mo>{</mo>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/*[3]"/>
<mo>|</mo>
<mrow>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/om:OMBVAR/om:OMV"/>
<mo>&#x2208;</mo>
<xsl:apply-templates select="following-sibling::*[2]"/>
<mo>&#x2227;</mo>
<xsl:apply-templates select="following-sibling::om:OMBIND[2]/*[3]">
 <xsl:with-param name="p" select="10"/>
</xsl:apply-templates>
</mrow>
<mo>}</mo>
</mrow>
</xsl:template>


<xsl:template match="om:OMS[@cd='set3' and @name='map_with_target_and_condition'][following-sibling::om:OMBIND[2]]">
<mrow>
<mo>{</mo>
<mrow>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/*[3]"/>
<mo>&#x2208;</mo>
<xsl:apply-templates select="following-sibling::*[3]"/>
</mrow>
<mo>|</mo>
<mrow>
<xsl:apply-templates select="following-sibling::om:OMBIND[1]/om:OMBVAR/om:OMV"/>
<mo>&#x2208;</mo>
<xsl:apply-templates select="following-sibling::*[2]"/>
<mo>&#x2227;</mo>
<xsl:apply-templates select="following-sibling::om:OMBIND[2]/*[3]">
 <xsl:with-param name="p" select="10"/>
</xsl:apply-templates>
</mrow>
<mo>}</mo>
</mrow>
</xsl:template>




</xsl:stylesheet>
