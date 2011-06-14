
<!--

Use and distribution of this code are permitted under the terms of the <a
href="http://www.w3.org/Consortium/Legal/copyright-software-19980720"
>W3C Software Notice and License</a>.

David Carlisle July 2009

for details of popcorn format, see
http://java.symcomp.org/FormalPopcorn.html
-->

<xsl:stylesheet version="2.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:om="http://www.openmath.org/OpenMath"
		>

<xsl:import href="verb.xsl"/>

<xsl:output method="text"/>


<xsl:template match="/">
  <xsl:for-each select="//om:OMOBJ">
    <xsl:text>&#10;</xsl:text>
    <xsl:apply-templates mode="pop" select="."/>
    <xsl:text>&#10;</xsl:text>
  </xsl:for-each>
</xsl:template>


<xsl:template match="*" mode="pop">
  <xsl:message select="'@@@@',name()"/>
</xsl:template>

<xsl:template match="om:OMOBJ" mode="pop">
  <xsl:apply-templates mode="pop"/>
</xsl:template>


<xsl:template match="om:OMR" mode="pop">
  <xsl:if test="not(starts-with(@href,'#'))">##</xsl:if>
  <xsl:value-of select="@href"/>
</xsl:template>

<xsl:template match="om:OMATTR" mode="pop">
  <xsl:apply-templates mode="pop" select="*[last()]"/>
  <xsl:apply-templates mode="pop" select="om:OMATP"/>
</xsl:template>

<xsl:template match="om:OMATP" mode="pop">
  <xsl:text>{</xsl:text>
  <xsl:for-each select="*[position() mod 2 = 1]">
    <xsl:apply-templates select="." mode="pop"/>
    <xsl:text> -&gt; </xsl:text>
    <xsl:apply-templates select="following-sibling::*[1]" mode="pop"/>
    <xsl:if test="position()!=last()">, </xsl:if>
  </xsl:for-each>
  <xsl:text>}</xsl:text>
</xsl:template>



<xsl:template match="om:OMA" mode="pop">
  <xsl:apply-templates select="*[1]" mode="pop"/>
  <xsl:text>(</xsl:text>
  <xsl:for-each select="*[position()!=1]">
    <xsl:apply-templates select="." mode="pop"/>
    <xsl:if test="position()!=last()">, </xsl:if>
  </xsl:for-each>
  <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="om:OMA[*[1]/@id]" mode="pop" priority="2">
  <xsl:apply-templates select="*[1]" mode="pop"/>
  <xsl:text>(</xsl:text>
  <xsl:for-each select="*[position()!=1]">
    <xsl:apply-templates select="." mode="pop"/>
    <xsl:if test="position()!=last()">, </xsl:if>
  </xsl:for-each>
  <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="*[@id]" mode="pop">
  <xsl:next-match/>
  <xsl:text>:</xsl:text>
  <xsl:value-of select="@id"/>
</xsl:template>

<xsl:template match="om:OME" mode="pop">
  <xsl:apply-templates select="*[1]" mode="pop"/>
  <xsl:text>!(</xsl:text>
  <xsl:for-each select="*[position()!=1]">
    <xsl:apply-templates select="." mode="pop"/>
    <xsl:if test="position()!=last()">, </xsl:if>
  </xsl:for-each>
  <xsl:text>)</xsl:text>
</xsl:template>


<xsl:template match="om:OMBIND" mode="pop">
  <xsl:apply-templates select="*[1]" mode="pop"/>
  <xsl:text>[</xsl:text>
  <xsl:apply-templates select="om:OMBVAR" mode="pop"/>
  <xsl:text> -&gt; </xsl:text>
  <xsl:apply-templates select="*[last()]" mode="pop"/>
  <xsl:text>]</xsl:text>
</xsl:template>


<xsl:template match="om:OMBVAR" mode="pop">
  <xsl:for-each select="*">
    <xsl:apply-templates select="." mode="pop"/>
    <xsl:if test="position()!=last()">, </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="om:OMV" mode="pop">
  <xsl:text>$</xsl:text>
  <xsl:value-of select="normalize-space(@name)"/>
</xsl:template>


<xsl:template match="om:OMS" mode="pop">
  <xsl:value-of select="normalize-space(@cd)"/>
  <xsl:text>.</xsl:text>
  <xsl:value-of select="normalize-space(@name)"/>
</xsl:template>

<xsl:template match="om:OMS[@cd='transc1']" mode="pop">
  <xsl:value-of select="normalize-space(@name)"/>
</xsl:template>


<xsl:template match="om:OMI" mode="pop">
  <xsl:value-of select="normalize-space(.)"/>
</xsl:template>

<xsl:template match="om:OMF[@dec]" mode="pop">
  <xsl:value-of select="normalize-space(@dec)"/>
</xsl:template>

<xsl:template match="om:OMF[@hex]" mode="pop">
  <xsl:text>0f</xsl:text>
  <xsl:value-of select="normalize-space(@hex)"/>
</xsl:template>


<xsl:template match="om:OMB" mode="pop">
  <xsl:text>%</xsl:text>
  <xsl:value-of select="."/>
  <xsl:text>%</xsl:text>
</xsl:template>

<xsl:template match="om:OMSTR" mode="pop">
  <xsl:text>"</xsl:text>
  <xsl:value-of select="replace(.,'&quot;','\\&quot;')"/>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="om:OMFOREIGN" mode="pop">
  <xsl:text>`</xsl:text>
  <xsl:value-of select="replace(@encoding,'`','\\`')"/>
  <xsl:apply-templates mode="verb"/>
  <xsl:text>`</xsl:text>
</xsl:template>





<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='prog1' and @name='block']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="10"/>
    <xsl:with-param name="op" select="';'"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='prog1' and @name='assign']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="20"/>
    <xsl:with-param name="op" select="':='"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='logic1' and @name='implies']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="30"/>
    <xsl:with-param name="op" select="'==&gt;'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='equivalent']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="40"/>
    <xsl:with-param name="op" select="'&lt;=&gt;'"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='logic1' and @name='or']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="50"/>
    <xsl:with-param name="op" select="'&gt;'"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='logic1' and @name='and']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="60"/>
    <xsl:with-param name="op" select="'and'"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='ge']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="70"/>
    <xsl:with-param name="op" select="'&gt;='"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='lt']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="80"/>
    <xsl:with-param name="op" select="'&lt;'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='leq']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="90"/>
    <xsl:with-param name="op" select="'&lt;='"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='gt']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="100"/>
    <xsl:with-param name="op" select="'&gt;'"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='eq']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="110"/>
    <xsl:with-param name="op" select="'='"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='geq']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="120"/>
    <xsl:with-param name="op" select="'&gt;='"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='relation1' and @name='neq']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="130"/>
    <xsl:with-param name="op" select="'!='"/>
  </xsl:call-template>
</xsl:template>

<!-- 140 <> -->

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='interval1' and @name='interva;']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="150"/>
    <xsl:with-param name="op" select="'..'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='plus']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="160"/>
    <xsl:with-param name="op" select="'+'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='minus']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="170"/>
    <xsl:with-param name="op" select="'-'"/>
  </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='times']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="180"/>
    <xsl:with-param name="op" select="'*'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='divide']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="190"/>
    <xsl:with-param name="op" select="'/'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='power']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="200"/>
    <xsl:with-param name="op" select="'^'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='complex1' and @name='complex_cartesian']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="210"/>
    <xsl:with-param name="op" select="'|'"/>
  </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='nums1' and @name='rational']]]" mode="pop">
  <xsl:param name="currentp" select="0"/>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="$currentp"/>
    <xsl:with-param name="p" select="220"/>
    <xsl:with-param name="op" select="'//'"/>
  </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='list1' and @name='list']]]" mode="pop">
  <xsl:text>[</xsl:text>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="0"/>
    <xsl:with-param name="p" select="0"/>
    <xsl:with-param name="op" select="','"/>
  </xsl:call-template>
  <xsl:text>]</xsl:text>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='set1' and @name='set']]]" mode="pop">
  <xsl:text>{</xsl:text>
  <xsl:call-template name="infix-popcorn">
    <xsl:with-param name="currentp" select="0"/>
    <xsl:with-param name="p" select="0"/>
    <xsl:with-param name="op" select="','"/>
  </xsl:call-template>
  <xsl:text>}</xsl:text>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='prog1' and @name='if']]]" mode="pop">
  <xsl:text> if </xsl:text>
  <xsl:apply-templates select="*[2]" mode="pop"/>
  <xsl:text> then </xsl:text>
  <xsl:apply-templates select="*[3]" mode="pop"/>
  <xsl:text> else </xsl:text>
  <xsl:apply-templates select="*[4]" mode="pop"/>
  <xsl:text> endif </xsl:text>
</xsl:template>

<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='prog1' and @name='while']]]" mode="pop">
  <xsl:text> while </xsl:text>
  <xsl:apply-templates select="*[2]" mode="pop"/>
  <xsl:text> do </xsl:text>
  <xsl:apply-templates select="*[3]" mode="pop"/>
  <xsl:text> endwhile </xsl:text>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='logic1' and @name='not']]]" mode="pop">
  <xsl:text> not(</xsl:text>
  <xsl:apply-templates select="*[2]" mode="pop"/>
  <xsl:text>)</xsl:text>
</xsl:template>


<xsl:template match="om:OMA[*[1][self::om:OMS[@cd='arith1' and @name='unary_minus']]]" mode="pop">
  <xsl:text> -(</xsl:text>
  <xsl:apply-templates select="*[2]" mode="pop"/>
  <xsl:text>)</xsl:text>
</xsl:template>


<xsl:template name="infix-popcorn">
  <xsl:param name="p" select="0"/>
  <xsl:param name="currentp" select="0"/>
  <xsl:param name="op"/>
  <xsl:if test="$currentp &gt; $p">(</xsl:if>
  <xsl:for-each select="*[position()!=1]">
    <xsl:apply-templates mode="pop" select=".">
      <xsl:with-param name="currentp" select="$p"/>
    </xsl:apply-templates>
    <xsl:if test="position()!=last()">
      <xsl:value-of select="'',$op,''"/>
    </xsl:if>
  </xsl:for-each>
  <xsl:if test="$currentp &gt; $p">)</xsl:if>
</xsl:template>

</xsl:stylesheet>