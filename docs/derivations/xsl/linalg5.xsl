

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>

<xsl:param name="big-matrix-size" select="6"/>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='zero']" >
 <xsl:choose>
  <xsl:when test="following-sibling::om:OMI[2] and not(following-sibling::om:OMI &gt;= $big-matrix-size)">
  <mfenced>
   <mtable>
   <xsl:variable name="x" select="following-sibling::om:OMI[1]"/>
   <xsl:variable name="y" select="following-sibling::om:OMI[2]"/>
   <xsl:for-each select="(//node())[position()&lt;=$x]">
   <mtr>
   <xsl:for-each select="(//node())[position()&lt;=$y]">
    <mtd><mn>0</mn></mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and @name='diagonal_matrix']" >
 <xsl:choose>
  <xsl:when test="count(following-sibling::*) &lt; $big-matrix-size">
  <mfenced>
   <mtable>
   <xsl:variable name="x" select="following-sibling::*"/>
   <xsl:for-each select="$x">
   <xsl:variable name="y" select="position()"/>
   <mtr>
   <xsl:for-each select="$x">
    <mtd>
    <xsl:choose>
    <xsl:when test="position()=$y"><xsl:apply-templates select="."/></xsl:when>
    <xsl:otherwise> <mn>0</mn> </xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='scalar']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1][self::om:OMI and . &lt; $big-matrix-size ]">
  <mfenced>
   <mtable>
   <xsl:variable name="x" select="following-sibling::om:OMI[1]"/>
   <xsl:variable name="z" select="following-sibling::om:*[2]"/>
   <xsl:for-each select="(//node())[position()&lt;=$x]">
   <xsl:variable name="y" select="position()"/>
   <mtr>
   <xsl:for-each select="(//node())[position()&lt;=$x]">
    <mtd>
    <xsl:choose>
    <xsl:when test="position()=$y"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn> </xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='identity']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1][self::om:OMI and . &lt; $big-matrix-size ]">
  <mfenced>
   <mtable>
   <xsl:variable name="x" select="following-sibling::om:OMI[1]"/>
   <xsl:for-each select="(//node())[position()&lt;=$x]">
   <xsl:variable name="y" select="position()"/>
   <mtr>
   <xsl:for-each select="(//node())[position()&lt;=$x]">
    <mtd>
    <xsl:choose>
    <xsl:when test="position()=$y"><mn>1</mn></xsl:when>
    <xsl:otherwise> <mn>0</mn> </xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and (@name='banded' or @name='upper-Hessenberg' or @name='tridiagonal')]" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size">
    <xsl:for-each select="following-sibling::*[1]/*">
    <xsl:sort select="count(*)" data-type="number" order="descending"/>
    <xsl:if test="position()=1">
      <xsl:value-of select="count(*) - 1"/>
    </xsl:if>
    </xsl:for-each>
   </xsl:variable>
   <xsl:variable name="first" select="count(following-sibling::*[1]/*[2]/*) -1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$y &lt;= $x]/*[position()= 2+ $size - $first + $y - $x]/*[position()= 1+ $y]|
          $vec[$y &gt;  $x]/*[position()= 2+ $size - $first + $y - $x]/*[position()= 1+ $x]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and @name='lower-Hessenberg']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size">
    <xsl:for-each select="following-sibling::*[1]/*">
    <xsl:sort select="count(*)" data-type="number" order="descending"/>
    <xsl:if test="position()=1">
      <xsl:value-of select="count(*) - 1"/>
    </xsl:if>
    </xsl:for-each>
   </xsl:variable>
   <xsl:variable name="first" select="count(following-sibling::*[1]/*[2]/*) -1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="y" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="x" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$y &lt;= $x]/*[position()= 2+ $size - $first + $y - $x]/*[position()= 1+ $y]|
          $vec[$y &gt;  $x]/*[position()= 2+ $size - $first + $y - $x]/*[position()= 1+ $x]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and @name='upper-triangular']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) - 1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec/*[position()= 1 + $x]/*[position()&gt;1][position()= 1 + $y - $x]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='lower-triangular']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) - 1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec/*[position()= 1 + $x]/*[position()= 1+ $y]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and @name='symmetric']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) - 1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$x &lt;= $y]/*[position()= 1 + $x]/*[position()= 2+ $y - $x]|
          $vec[$x &gt;  $y]/*[position()=  1 + $y]/*[position()= 2+ $x - $y]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='skew-symmetric']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) "/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$x &lt;= $y]/*[position()= 1 + $x]/*[position()!=1][position()=  $y - $x]|
          $vec[$x &gt;  $y]/*[position()=  1 + $y]/*[position()!=1][position()=  $x - $y]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z and $y &lt; $x">
          <mo>-</mo>
   <xsl:variable name="t">
          <xsl:apply-templates select="$z">
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
    </xsl:when>
    <xsl:when test="$z">
          <xsl:apply-templates select="$z"/>
    </xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>


<xsl:template match="om:OMS[@cd='linalg5'  and @name='Hermitian']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) - 1"/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$x &lt;= $y]/*[position()= 1 + $x]/*[position()= 2+ $y - $x]|
          $vec[$x &gt;  $y]/*[position()=  1 + $y]/*[position()= 2+ $x - $y]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z and $y &lt; $x">
           <mover><xsl:apply-templates select="$z"/><mo>&#xaf;</mo></mover></xsl:when>
    <xsl:when test="$z"><xsl:apply-templates select="$z"/></xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

<xsl:template match="om:OMS[@cd='linalg5'  and @name='anti-Hermitian']" >
 <xsl:choose>
  <xsl:when test="following-sibling::*[1]/*[1][self::om:OMS[@name='vector']] 
           and not(following-sibling::*[1]/*/*[$big-matrix-size + 1])">
   <xsl:variable name="size" select="count(following-sibling::*[1]/*) "/>
   <xsl:variable name="vec" select="following-sibling::*[1]"/>
  <mfenced>
   <mtable>
   <xsl:for-each select="(//node())[position()&lt;=$size]">
   <xsl:variable name="x" select="position()"/>
   <mtr>
     <xsl:for-each select="(//node())[position()&lt;=$size]">
     <xsl:variable name="y" select="position()"/>
    <xsl:variable name="z" 
  select="$vec[$x &lt;= $y]/*[position()= 1 + $x]/*[position()!=1][position()=  $y - $x]|
          $vec[$x &gt;  $y]/*[position()=  1 + $y]/*[position()!=1][position()=  $x - $y]"/>
    <mtd>
    <xsl:choose>
    <xsl:when test="$z and $y &lt; $x">
          <mo>-</mo>
          <mfenced>
          <mover>
          <xsl:apply-templates select="$z"/>
          <mo>&#xaf;</mo>
          </mover>
          </mfenced>
    </xsl:when>
    <xsl:when test="$z">
          <xsl:apply-templates select="$z"/>
    </xsl:when>
    <xsl:otherwise> <mn>0</mn></xsl:otherwise>
    </xsl:choose>
    </mtd>
   </xsl:for-each>
   </mtr>
   </xsl:for-each>
   </mtable>
  </mfenced>
  </xsl:when>
  <xsl:otherwise>
    <xsl:call-template name="prefix"/>
  </xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>

