

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<xsl:key name="id" match="*[@id]" use="@id"/>


<xsl:template match="om:OMOBJ">
  <math display="block">
    <xsl:apply-templates/>
  </math>
</xsl:template>

<xsl:template match="om:OMATTR">
<xsl:apply-templates mode="attrib" select="(om:OMATP/*[position() mod
2 = 1]|*[2])[1]"/>
</xsl:template>

<xsl:template mode="attrib" match="om:OMATP/om:OMS" priority="0.4">
  <xsl:message>[attrib: <xsl:value-of
       select="@cd"/>:<xsl:value-of select="@name"/>]</xsl:message>
<xsl:apply-templates mode="attrib" select="(following-sibling::*[position() mod
2 = 0]|../../*[2])[1]"/>
</xsl:template>

<xsl:template mode="attrib" match="om:OMATTR/*">
  <xsl:message>[attributed: <xsl:value-of
       select="name()"/>]</xsl:message>
 <xsl:apply-templates select="."/>
</xsl:template>

<xsl:template match="om:OMA">
  <xsl:param name="p" select="0"/>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
  <mrow>
  <mo>(</mo>
        <xsl:apply-templates select="*[1]"/>
  <mo>)</mo>
  </mrow>
  <mo>&#x2061;</mo>
  <mrow>
  <mo>(</mo>
  <xsl:for-each select="following-sibling::*">
   <xsl:apply-templates select="."/>
   <xsl:if test="position() &lt; last()"><mo>,</mo></xsl:if>
  </xsl:for-each>
  <mo>)</mo>
  </mrow>
  </mrow>
  </xsl:when>
  <xsl:otherwise>
    <xsl:apply-templates select="*[1]">
      <xsl:with-param name="p" select="$p"/>
    </xsl:apply-templates>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="om:OMBIND">
  <xsl:param name="p" select="0"/>
    <xsl:apply-templates mode="ombind" select="*[1]">
      <xsl:with-param name="p" select="$p"/>
    </xsl:apply-templates>
</xsl:template>

<xsl:template match="om:OMBVAR">
  <mrow>
    <xsl:for-each select="*">
      <xsl:apply-templates select="."/>
      <xsl:if test="not(position()=last())"><mo>,</mo></xsl:if>
    </xsl:for-each>
  </mrow>
</xsl:template>


<xsl:template match="om:OMV">
  <xsl:call-template name="prefix">
   <xsl:with-param name="mo">
  <mi>
    <xsl:value-of select="@name"/>
  </mi>
</xsl:with-param>
 </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMSTR">
  <mtext>
    <xsl:value-of select="."/>
  </mtext>
</xsl:template>

<xsl:template match="om:OMR">
  <xsl:variable name="id" select="substring-after(@href,'#')"/>
  <xsl:for-each select="document(substring-before(@href,'#'),.)">
  <xsl:apply-templates select="key('id',$id)"/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="om:OME">
  <merror>
    <xsl:apply-templates/>
  </merror>
</xsl:template>


<xsl:template match="om:OMI">
  <mn>
    <xsl:value-of select="."/>
  </mn>
</xsl:template>

<xsl:template match="om:OMF[@dec]">
  <mn>
    <xsl:value-of select="@dec"/>
  </mn>
</xsl:template>

<xsl:template match="om:OMF[@dec='INF']" priority="2">
  <mn>&#x221E;</mn>
</xsl:template>
<xsl:template match="om:OMF[@dec='-INF']" priority="2">
  <mn>-&#x221E;</mn>
</xsl:template>

<xsl:template match="om:OMF[@hex]">
  <mrow>
    <mi>double</mi>
    <mfenced>
    <mn>0x<xsl:value-of select="@hex"/></mn>
   </mfenced>
  </mrow>
</xsl:template>


<xsl:template match="om:OMS">
<!-- <xsl:message>[<xsl:value-of select="@cd"/>:<xsl:value-of
select="@name"/>]</xsl:message> -->
  <xsl:call-template name="prefix">
    <xsl:with-param name="mo"><mi><xsl:value-of select="@name"/></mi></xsl:with-param>
  </xsl:call-template>
</xsl:template>



<xsl:template name="prefix">
  <xsl:param name="p" select="0"/>
  <xsl:param name="mo">
    <mi><xsl:value-of select="@name"/></mi>
  </xsl:param>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
    <xsl:copy-of select="$mo"/>
  <mo>&#x2061;</mo>
  <mrow>
  <mo>(</mo>
  <xsl:for-each select="following-sibling::*">
   <xsl:apply-templates select="."/>
   <xsl:if test="position() &lt; last()"><mo>,</mo></xsl:if>
  </xsl:for-each>
  <mo>)</mo>
  </mrow>
  </mrow>
  </xsl:when>
  <xsl:otherwise>
    <xsl:copy-of select="$mo"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="infix" >
  <xsl:param name="mo"/>
  <xsl:param name="p" select="0"/>
  <xsl:param name="this-p" select="0"/>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
  <xsl:if test="$this-p &lt; $p"><mo>(</mo></xsl:if>
  <xsl:for-each select="following-sibling::*">
   <xsl:if test="position() &gt; 1">
    <xsl:copy-of select="$mo"/>
   </xsl:if>   
   <xsl:apply-templates select=".">
     <xsl:with-param name="p" select="$this-p"/>
   </xsl:apply-templates>
  </xsl:for-each>
  <xsl:if test="$this-p &lt; $p"><mo>)</mo></xsl:if>
  </mrow>
  </xsl:when>
  <xsl:otherwise>
    <xsl:copy-of select="$mo"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="binary" >
  <xsl:param name="mo"/>
  <xsl:param name="p" select="0"/>
  <xsl:param name="this-p" select="0"/>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
  <xsl:if test="$this-p &lt; $p"><mo>(</mo></xsl:if>
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="$this-p"/>
   </xsl:apply-templates>
   <xsl:copy-of select="$mo"/>
   <xsl:apply-templates select="following-sibling::*[2]">
     <xsl:with-param name="p" select="$this-p"/>
   </xsl:apply-templates>
  <xsl:if test="$this-p &lt; $p"><mo>)</mo></xsl:if>
  </mrow>
  </xsl:when>
  <xsl:otherwise>
    <xsl:copy-of select="$mo"/>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template name="msup">
  <xsl:param name="base">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
  </xsl:param>
  <xsl:param name="script">
   <xsl:apply-templates select="following-sibling::*[2]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
  </xsl:param>
   <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <msup>
    <xsl:copy-of select="$base"/>
    <xsl:copy-of select="$script"/>
   </msup>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template name="msub">
  <xsl:param name="base">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
  </xsl:param>
  <xsl:param name="script">
   <xsl:apply-templates select="following-sibling::*[2]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
  </xsl:param>
   <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <msub>
    <xsl:copy-of select="$base"/>
    <xsl:copy-of select="$script"/>
   </msub>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="fenced">
  <xsl:param name="open"><mo>(</mo></xsl:param>
  <xsl:param name="close"><mo>)</mo></xsl:param>
  <xsl:param name="sep"><mo separator="true">,</mo></xsl:param>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
    <xsl:copy-of select="$open"/>
    <xsl:for-each select="following-sibling::*">
    <xsl:apply-templates select="."/>
    <xsl:if test="position()!=last()">
        <xsl:copy-of select="$sep"/>
    </xsl:if>
    </xsl:for-each>
    <xsl:copy-of select="$close"/>
  </mrow>
    </xsl:when>
    <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="*" mode="ombind">
 <xsl:call-template name="ombind">
  <xsl:with-param name="mo">
    <xsl:apply-templates select="."/>
  </xsl:with-param>
 </xsl:call-template>
</xsl:template>

<xsl:template name="ombind">
  <xsl:param name="mo"/>
  <xsl:param name="p" select="0"/>
  <xsl:param name="this-p" select="0"/>
 <mrow>
  <xsl:copy-of select="$mo"/>
  <mo>&#x2009;</mo>
  <xsl:apply-templates select="following-sibling::om:OMBVAR[1]"/>
  <mo>.</mo>
  <xsl:apply-templates select="following-sibling::*[2]"/>
 </mrow>
</xsl:template>

</xsl:stylesheet>
