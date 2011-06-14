

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<xsl:template mode="attrib" match="om:OMS[@cd='sts1' and
@name='type']">
<mrow>
<xsl:apply-templates select="../../*[2]"/>
<msub><mo>&#x2208;</mo><mi>sts</mi></msub>
<xsl:apply-templates select="following-sibling::*[1]"/>
</mrow>
</xsl:template>

<xsl:template match="om:OMS[@cd='sts' and @name='mapsto']"  >
  <xsl:param name="p"/>
   <mrow>
    <xsl:choose>
     <xsl:when test="following-sibling::*[3]">
      <mfenced>
       <xsl:apply-templates select="following-sibling::*[position()&lt;last()]"/>
      </mfenced>
     </xsl:when>
     <xsl:otherwise>
      <mrow>
       <xsl:apply-templates select="following-sibling::*[position()&lt;last()]"/>
      </mrow>
     </xsl:otherwise>
    </xsl:choose>
     <mo>&#x2192;</mo>
       <xsl:apply-templates select="following-sibling::*[last()]"/>
   </mrow>
</xsl:template>


</xsl:stylesheet>



