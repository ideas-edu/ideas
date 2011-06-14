

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>


<xsl:template match="om:OMS[@cd='permgp2' and @name='symmetric_group']"  >
  <xsl:call-template name="msub">
    <xsl:with-param name="base"><mi>S</mi></xsl:with-param>
    <xsl:with-param name="script">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='permgp2' and @name='alternating_group']"  >
  <xsl:call-template name="msub">
    <xsl:with-param name="base"><mi>Alt</mi></xsl:with-param>
    <xsl:with-param name="script">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='permgp2' and @name='cyclic_group']"  >
  <xsl:call-template name="msub">
    <xsl:with-param name="base"><mi>C</mi></xsl:with-param>
    <xsl:with-param name="script">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='permgp2' and @name='dihedral_group']"  >
  <xsl:call-template name="msub">
    <xsl:with-param name="base"><mi>D</mi></xsl:with-param>
    <xsl:with-param name="script">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>
