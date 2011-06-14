<xsl:stylesheet 
  version="1.0"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns:sts="http://www.openmath.org/OpenMathCDS"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.w3.org/1999/xhtml">

<!-- verb mode -->
<xsl:import href="verb.xsl"/>

<xsl:output method="xml" indent="yes"/>


<xsl:template match="sts:CDSignatures">
  <html>
  <head>
    <title><xsl:value-of select="@cd"/></title>
    <link rel="stylesheet" href="../cd/omcd.css" type="text/css"/>
  </head>
  <body>
    <h1>OpenMath Signatures</h1>
    <hr/>
    <p>
      Type System:
      <a href="../cd/{@type}.html"><xsl:value-of select="@type"/></a>
      <br/>
      Content Dictionary:
      <a href="../cd/{@cd}.xhtml"><xsl:value-of select="@cd"/></a>
      <br/>
      CD Signature File:
      <a href="{@cd}.sts"><xsl:value-of select="@cd"/>.sts</a>
<!--
      <br/>
      CD Signature as XML Encoded OpenMath: 
      <a href="{@cd}.omsts"><xsl:value-of select="@cd"/>.omsts</a>
-->
    </p>
    <hr/>
    <xsl:apply-templates/>
  </body>
  </html>
</xsl:template>

<xsl:template match="sts:CDSComment">
  <pre>
  <b>
  <xsl:apply-templates/>
  </b>
  </pre>
</xsl:template>


<xsl:template match="sts:Signature">
  <hr/>
  <h2><a name="{@name}"><xsl:value-of select="@name"/></a></h2>
  <pre>
  <xsl:apply-templates mode="verb"/>
  </pre>
  <xsl:apply-templates mode="display"/>
  <p>
     <a href="../cd/{../@cd}.xhtml#{@name}">Content Dictionary Entry.</a> 
  </p>
  <hr/>
  <table width="100%">
    <tr>
    <td align="right"><font size="-1">
    <xsl:variable name="n"
        select="normalize-space(following-sibling::Signature[1]/@name)"/>
      <xsl:choose>
        <xsl:when test="''=$n">
          <xsl:variable name="n2"
              select="normalize-space(../Signature[1]/@name)"/>
          [First:
          <a href="#{$n2}"><xsl:value-of select="$n"/></a>]
        </xsl:when>
        <xsl:otherwise>
          [Next:
          <a href="#{$n}"><xsl:value-of select="$n"/></a>]
        </xsl:otherwise>
      </xsl:choose>
    <xsl:variable name="p"
       select="normalize-space(preceding-sibling::Signature[1]/@name)"/>
      <xsl:choose>
        <xsl:when test="''=$p">
          <xsl:variable name="p2"
              select="normalize-space(../Signature[last()]/@name)"/>
          [Last:
          <a href="#{$p2}"><xsl:value-of select="$p"/></a>]
        </xsl:when>
        <xsl:otherwise>
          [Previous:
          <a href="#{$p}"><xsl:value-of select="$p"/></a>]
        </xsl:otherwise>
      </xsl:choose>
      [<a href="#top">Top</a>]
    </font>
    </td>
    </tr>
  </table>
  <hr/>
</xsl:template>





<!--  display mode    -->

<xsl:template mode="display" match="om:OMOBJ">
  <xsl:apply-templates mode="display"/>
</xsl:template>



<xsl:template mode="display" match="om:OMA[om:OMS[@name='mapsto']]">
(
<xsl:apply-templates mode="display" select="*[not(position()=last())]"/>
 >>
<xsl:apply-templates mode="display"  select="*[position()=last()]"/>
)
</xsl:template>

<xsl:template mode="display" match="om:OMA[om:OMS[@name='nassoc']]">
(
<xsl:apply-templates mode="display"  select="*[position()=last()]"/>
<sup>*assoc</sup>
)
</xsl:template>

<xsl:template mode="display" match="om:OMA[om:OMS[@name='nary']]">
(
<xsl:apply-templates mode="display"  select="*[position()=last()]"/>
<sup>*</sup>
)
</xsl:template>



<xsl:template mode="display" match="om:OMV">
  <xsl:text> </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text> </xsl:text>
</xsl:template>



<xsl:template mode="display" match="om:OMS[not(@cd='sts')]">
  <xsl:text> </xsl:text>
  <a href="../cd/{@cd}.xhtml#{@name}"><xsl:value-of select="@name"/></a>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template mode="display"
      match="om:OMS[@name='attribution']|
             om:OMS[@name='binder']|
             om:OMS[@name='Object']|
             om:OMS[@name='NumericalValue']">
  <xsl:text> </xsl:text>
  <b><xsl:value-of select="@name"/></b>
  <xsl:text> </xsl:text>
</xsl:template>




</xsl:stylesheet>

