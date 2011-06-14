

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='logic1' and @name='and']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2227;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMS[@cd='logic1' and @name='or']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2228;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='logic1' and @name='xor']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>xor</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='logic1' and @name='equivalent']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2261;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='logic1' and @name='implies']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&#x21D2;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="0"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='logic1' and @name='not']" >
<mo>&#xAC;</mo>
<xsl:apply-templates select="following-sibling::*">
<xsl:with-param name="p" select="5"/>
</xsl:apply-templates>
</xsl:template>


<xsl:template match="om:OMS[@cd='logic1' and @name='true']"  >
 <mi>T</mi>
</xsl:template>

<xsl:template match="om:OMS[@cd='logic1' and @name='false']"  >
 <mi>F</mi>
</xsl:template>

</xsl:stylesheet>



