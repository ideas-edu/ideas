

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>


<!-- modify plus to special case a + -b to a - b 
<xsl:template match="om:OMS[@cd='arith1' and @name='plus']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>+</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

-->

<xsl:template match="om:OMS[@cd='arith1' and @name='plus']"  >
  <xsl:param name="p"/>
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
  <mrow>
  <xsl:if test="1 &lt; $p"><mo>(</mo></xsl:if>
  <xsl:for-each select="following-sibling::*">
   <xsl:choose>
   <xsl:when test="position() &gt; 1 and 
             self::om:OMI[translate(.,' ','') &lt; 0]">
    <mo>-</mo>
    <mn><xsl:value-of select="translate(.,' -','')"/></mn>
   </xsl:when>
   <xsl:when test="position() &gt; 1 and 
             self::om:OMA/*[1][self::om:OMS[@name='unary_minus']]">
    <mo>-</mo>
   <xsl:apply-templates select="*[2]">
     <xsl:with-param name="p" select="1"/>
   </xsl:apply-templates>
   </xsl:when>

   <xsl:when test="position() &gt; 1 and 
             self::om:OMA[*[position()=1 and last()=3]
                         [self::om:OMS and @name='times']]/
              *[2][self::om:OMI[translate(.,' ','') &lt; 0]]">
    <mo>-</mo>
   <mrow>
   <mn><xsl:value-of select="translate(*[2],' -','')"/></mn>
   <xsl:apply-templates select="*[3]">
     <xsl:with-param name="p" select="1"/>
   </xsl:apply-templates>
   </mrow>
   </xsl:when>

   <xsl:when test="position() &gt; 1 and 
             self::om:OMA[*[position()=1 and last()=3]
                         [self::om:OMS and @name='times']]/
              *[2][self::om:OMA]/*[1][self::om:OMS[@name='unary_minus']]">
    <mo>-</mo>
   <mrow>
   <xsl:apply-templates select="*[2]/*[2]">
   <xsl:with-param name="p" select="1"/>
   </xsl:apply-templates>
   <xsl:apply-templates select="*[3]">
     <xsl:with-param name="p" select="1"/>
   </xsl:apply-templates>
   </mrow>
   </xsl:when>
   <xsl:otherwise>
   <xsl:if test="position() &gt; 1">
    <mo>+</mo>
   </xsl:if>   
   <xsl:apply-templates select=".">
     <xsl:with-param name="p" select="1"/>
   </xsl:apply-templates>
   </xsl:otherwise>
  </xsl:choose>
  </xsl:for-each>
  <xsl:if test="1 &lt; $p"><mo>)</mo></xsl:if>
  </mrow>
  </xsl:when>
  <xsl:otherwise>
    <mo>+</mo>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[(@cd='arith1' or @cd='arith2') and @name='times']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo">
    <mo>
    <xsl:choose>
    <xsl:when test="preceding-sibling::*">&#xD7;</xsl:when>
    <xsl:when test="following-sibling::om:OMS[string-length(@name)!=1][2]">&#xD7;</xsl:when>
    <xsl:when test="following-sibling::*[not(self::om:OMI or
  self::om:OMF or self::om:OMS[@cd='alg1'])]"
    >&#x2062;<!-- IT --></xsl:when>
    <xsl:otherwise>&#xD7;</xsl:otherwise>
    </xsl:choose>
    </mo>
   </xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="2"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='arith1' and @name='divide']"  >
  <xsl:param name="p"/>
  <xsl:param name="inline" select="false()"/>
  <xsl:choose>
  <xsl:when test="$inline">
   <mrow>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <mo>/</mo>
   <xsl:apply-templates select="following-sibling::*[2]"/>
   </mrow>
  </xsl:when>
  <xsl:when test="$p &gt; 2">
  <mfenced>
  <mfrac>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <xsl:apply-templates select="following-sibling::*[2]"/>
  </mfrac>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
  <mfrac>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <xsl:apply-templates select="following-sibling::*[2]"/>
  </mfrac>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='arith1' and @name='minus']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>-</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

<!-- not really in arith1 -->

<xsl:template match="om:OMS[@cd='arith1' and @name='plusminus']"  >
  <xsl:param name="p"/>
  <xsl:call-template name="infix">
    <xsl:with-param name="mo"><mo>&#xB1;</mo></xsl:with-param>
    <xsl:with-param name="p" select="$p"/>
    <xsl:with-param name="this-p" select="1"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='arith1' and @name='unary_minus']"  >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <mrow>
   <mo>-</mo>
   <xsl:variable name="t">
   <xsl:apply-templates select="following-sibling::*[1]">
     <xsl:with-param name="p" select="100"/>
   </xsl:apply-templates>
   </xsl:variable>
   <xsl:choose>
   <xsl:when test="starts-with(normalize-space($t),'-')">
     <mfenced><xsl:copy-of select="$t"/></mfenced>
   </xsl:when>
   <xsl:otherwise>
     <xsl:copy-of select="$t"/>
   </xsl:otherwise>
   </xsl:choose>
   </mrow>
  </xsl:when>
  <xsl:otherwise>
   <mo>-</mo>
  </xsl:otherwise>
 </xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='arith1' and @name='power']"  >
  <xsl:call-template name="msup"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='arith1' and @name='conjugate']"  >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <mover>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <mo>&#xaf;</mo>
   </mover>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='arith1' and @name='abs']"  >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <mrow>
   <mo>|</mo>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   <mo>|</mo>
   </mrow>
  </xsl:when>
  <xsl:otherwise>
    <mi>abs</mi>
  </xsl:otherwise>
  </xsl:choose>
</xsl:template>



<xsl:template match="om:OMS[@cd='arith1' and @name='root']"  >
  <xsl:choose>
  <xsl:when test="parent::om:OMA and not(preceding-sibling::*)">
   <xsl:choose>
   <xsl:when test="following-sibling::*[2]/self::om:OMI[normalize-space(.)='2']">
   <msqrt>
     <xsl:apply-templates select="following-sibling::*[1]"/>
   </msqrt>
   </xsl:when>
   <xsl:otherwise>
   <mroot>
   <xsl:apply-templates select="following-sibling::*"/>
   </mroot>
   </xsl:otherwise>
   </xsl:choose>
   </xsl:when>
   <xsl:otherwise>
   <mi><xsl:value-of select="@name"/></mi>
   </xsl:otherwise>
  </xsl:choose>
</xsl:template>


<xsl:template
  match="om:OMS[@cd='arith1' and (@name='sum' or @name='product')]"  >
   <xsl:choose>
<!--
If the body is a lambda expression, put the bound variable in the
subscript, and just typeset the body of the lambda expression,
zap the lambda.
-->
   <xsl:when test="following-sibling::*[2]/self::om:OMBIND/*[1][self::om:OMS[@name='lambda']]">

   <xsl:choose>
<!--
If there is an explicit interval put limits top and bottom
else put range of summation at bottom
-->
   <xsl:when test="following-sibling::*[1]/self::om:OMA/*[1][self::om:OMS[@name='integer_interval']]">
   <munderover>
   <mo>
   <xsl:choose>
    <xsl:when test="@name='sum'">&#x2211;</xsl:when>
    <xsl:otherwise>&#x220F;</xsl:otherwise>
   </xsl:choose>
   </mo>
   <mrow>
   <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR"/>
   <mo>=</mo>
   <xsl:apply-templates select="following-sibling::om:OMA/*[2]"/>
   </mrow>
   <xsl:apply-templates select="following-sibling::om:OMA/*[3]"/>
   </munderover>
   </xsl:when>
   <xsl:otherwise>
   <munder>
   <mo>
   <xsl:choose>
    <xsl:when test="@name='sum'">&#x2211;</xsl:when>
    <xsl:otherwise>&#x220F;</xsl:otherwise>
   </xsl:choose>
   </mo>
   <mrow>
   <xsl:apply-templates select="following-sibling::om:OMBIND/om:OMBVAR"/>
   <mo>in</mo> 
   <xsl:apply-templates select="following-sibling::*[1]"/>
   </mrow>
   </munder>
   </xsl:otherwise>
   </xsl:choose>
   <xsl:apply-templates select="following-sibling::om:OMBIND/*[3]"/>
   </xsl:when>

<!--
 No lambda
-->
   <xsl:otherwise>
   <xsl:choose>
<!--
If there is an explicit interval put limits top and bottom
else put range of summation at bottom
-->
   <xsl:when test="following-sibling::*[1]/self::om:OMA/*[1][self::om:OMS[@name='integer_interval']]">
   <munderover>
   <mo>
   <xsl:choose>
    <xsl:when test="@name='sum'">&#x2211;</xsl:when>
    <xsl:otherwise>&#x220F;</xsl:otherwise>
   </xsl:choose>
   </mo>
   <xsl:apply-templates select="following-sibling::om:OMA/*[2]"/>
   <xsl:apply-templates select="following-sibling::om:OMA/*[3]"/>
   </munderover>
   </xsl:when>
   <xsl:otherwise>
   <msub>
   <mo>
   <xsl:choose>
    <xsl:when test="@name='sum'">&#x2211;</xsl:when>
    <xsl:otherwise>&#x220F;</xsl:otherwise>
   </xsl:choose>
   </mo>
   <xsl:apply-templates select="following-sibling::*[1]"/>
   </msub>
   </xsl:otherwise>
   </xsl:choose>
   <xsl:apply-templates select="following-sibling::*[2]"/>
   </xsl:otherwise>
   </xsl:choose>

</xsl:template>

</xsl:stylesheet>



