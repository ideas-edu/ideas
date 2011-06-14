

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:template match="om:OMS[@cd='relation1' and @name='eq']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>=</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='relation1' and @name='geq']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2265;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='relation1' and @name='leq']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2264;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='relation1' and @name='neq']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2260;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='relation1' and @name='lt']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&lt;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='relation1' and @name='gt']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>&gt;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='relation1' and @name='approx']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#x2248;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>

