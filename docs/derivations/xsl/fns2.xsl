
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>



<!-- currently only for make_list version -->
<xsl:template match="om:OMS[@cd='fns2' and @name='apply_to_list']
                           [following-sibling::*[1][@name='set']]" >
<xsl:apply-templates select="following-sibling::om:OMA/*[1]">
  <xsl:with-param name="open" select="'{'"/>
  <xsl:with-param name="close" select="'}'"/>
</xsl:apply-templates>
</xsl:template>

<xsl:template match="om:OMS[@cd='fns2' and @name='predicate_on_list']
                           [following-sibling::*[2][self::om:OMA/om:OMS[@name='list']]]" >
<xsl:variable name="op">
<xsl:apply-templates select="following-sibling::*[1]"/>
</xsl:variable>
<mrow>
<xsl:for-each select="following-sibling::*[2]/*[position()!=1]">
  <xsl:apply-templates select="."/>
  <xsl:if test="position()!=last()">
    <xsl:copy-of select="$op"/>
  </xsl:if>
</xsl:for-each>
</mrow>
</xsl:template>



</xsl:stylesheet>



