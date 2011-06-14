


<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='cartesian_product']" >
   <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#xD7;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='emptyset']" >
  <mi>&#x2205;</mi>
</xsl:template>


<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='in']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2208;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='subset']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2282;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='prsubset']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2282;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='notprsubset']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2284;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='notsubset']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2284;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='setdiff']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2216;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='notin']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x2209;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='union']" >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x222A;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='intersect']" >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2229;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='set']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>{</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>}</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='suchthat'][following-sibling::om:OMBIND/om:OMS/@name='lambda']">
  <mrow>
    <mo>{</mo>
    <mrow>
      <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR/om:OMV"/>
      <mo>&#x2208;</mo>
      <xsl:apply-templates select="following-sibling::*[1]"/>
    </mrow>
    <mo>|<!--&#x2223;--></mo>
    <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
   <mo>}</mo>
</mrow>
</xsl:template>





<xsl:template match="om:OMS[(@cd='set1' or @cd='multiset1') and @name='map'][following-sibling::om:OMBIND/om:OMS/@name='lambda']">
  <mrow>
    <mo>{</mo>
    <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
    <mo>|<!--&#x2223;--></mo>
    <mrow>
      <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR/om:OMV"/>
      <mo>&#x2208;</mo>
      <xsl:apply-templates select="following-sibling::*[2]"/>
    </mrow>
   <mo>}</mo>
</mrow>
</xsl:template>


</xsl:stylesheet>



