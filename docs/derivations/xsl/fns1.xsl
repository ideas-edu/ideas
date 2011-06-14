

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='fns1' and @name='identity']" >
  <xsl:call-template name="prefix">
   <xsl:with-param name="mo">
    <mi>Id</mi>
   </xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='fns1' and @name='inverse']" >
<xsl:call-template name="msup">
 <xsl:with-param name="script">
   <mn>-1</mn>
  </xsl:with-param>
</xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='fns1' and @name='left_compose']" >
  <xsl:param name="p"/>
  <xsl:call-template name="binary">
    <xsl:with-param name="mo"><mo>o</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='fns1' and @name='lambda']">
  <xsl:call-template name="prefix">
    <xsl:with-param name="mo"><mo>&#x3BB;</mo></xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='fns1' and @name='restriction']">
  <xsl:choose>
    <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
      <msub>
	<mrow>
	  <xsl:apply-templates select="following-sibling::*[1]"/>
	  <mo>|</mo>
	</mrow>
	<mrow>
	  <xsl:apply-templates select="following-sibling::*[2]"/>
	</mrow>
      </msub>
    </xsl:when>
    <xsl:otherwise>
      <mi>restriction</mi>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>



</xsl:stylesheet>



