

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:template match="om:OMS[@cd='setname2'  and @name='H']" >
  <mi mathvariant="double-struck">H</mi>
</xsl:template>


<xsl:template match="om:OMS[@cd='setname2' and @name='Zm']"  >
  <xsl:call-template name="msub">
   <xsl:with-param name="base">
    <mi mathvariant="double-struck">Z</mi>
   </xsl:with-param>
   <xsl:with-param name="script">
     <xsl:apply-templates select="following-sibling::*[1]"/>
   </xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname2' and @name='GFp']"  >
  <xsl:call-template name="msub">
   <xsl:with-param name="base">
    <mi mathvariant="double-struck">GF</mi>
   </xsl:with-param>
   <xsl:with-param name="script">
     <xsl:apply-templates select="following-sibling::*[1]"/>
   </xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='setname2' and @name='GFpn']"  >
  <xsl:call-template name="msub">
   <xsl:with-param name="base">
    <mi mathvariant="double-struck">GF</mi>
   </xsl:with-param>
   <xsl:with-param name="script">
     <msup>
     <xsl:apply-templates select="following-sibling::*[1]"/>
     <xsl:apply-templates select="following-sibling::*[2]"/>
     </msup>
   </xsl:with-param>
 </xsl:call-template>
</xsl:template>



</xsl:stylesheet>



