
<!-- OpenMath Validating stylesheet version 1.0
     Copyright 1999 David Carlisle, NAG
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0"
                >

<!-- 
USAGE (assuming xt, any XSL engine should work)
=====

xt file-containing-OpenMath omvalidate.xsl

The stylesheet skips over anything not contained in an OMOBJ element
and then reports on any elements that it suspects of being invalid.
It currently does not link back to the original source, this may be
added later.

It checks for each <OMS name= cd= /> that the name really is in the CD
this involves an http connection to
 http://openmath.nag.co.uk/openmath/cd/internal/cd/
giving a directory on the local disk which contains the CDs wil speed
things up, the syntax for this is

xt file-containing-OpenMath omvalidate.xsl cdhome=some-directory-of-cds

David Carlisle
  1999-11-22
-->

<xsl:output method="xml" indent="yes"/>
<xsl:strip-space elements="*"/>

<xsl:param name="cdhome" 
           select="'http://openmath.nag.co.uk/openmath/cd/internal/cd/'"/>

<xsl:template match="/">
  <xsl:apply-templates select="//OMOBJ"/>
</xsl:template>

<xsl:template match="*">
  <xsl:copy>
  <xsl:text>
  Unknown element</xsl:text>
  </xsl:copy>
</xsl:template>

<xsl:template match="OMS">
 <xsl:choose>
 <xsl:when test="not(@cd and @name and count(@*)=2)">
 <OMS><xsl:copy-of select="@*"/>
  <xsl:text>
  OMS must have just cd and name attributes</xsl:text>
 </OMS>
 </xsl:when>
 <xsl:when test="count(*) != 0 or not(. = '')">
 <OMS><xsl:copy-of select="@*"/>
  <xsl:text>
  OMS must be EMPTY</xsl:text>
 </OMS>
 </xsl:when>
 <xsl:when test=
  "not(document(concat($cdhome,@cd,'.ocd'))
     /CD/CDDefinition[normalize-space(Name) = current()/@name])">
 <OMS><xsl:copy-of select="@*"/>
  <xsl:text>
  Bad name: </xsl:text>
  <xsl:value-of select="@name"/>
 <xsl:text> not in </xsl:text>
  <xsl:value-of select="@cd"/>
 </OMS>
 </xsl:when>
 </xsl:choose>
</xsl:template>


<xsl:template match="OMV">
 <xsl:choose>
 <xsl:when test="not(@name and count(@*)=1)">
 <OMV><xsl:copy-of select="@*"/>
  <xsl:text>
  OMV must have just name attribute</xsl:text>
 </OMV>
 </xsl:when>
 <xsl:when test="count(*) != 0 or not(. = '')">
 <OMV><xsl:copy-of select="@*"/>
  <xsl:text>
  OMV must be EMPTY</xsl:text>
 </OMV>
 </xsl:when>
 </xsl:choose>
</xsl:template>

<xsl:template match="OMI|OMB|OMSTR">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OMI><xsl:copy-of select="@*"/>
  <xsl:text>
  OMI does not take attributes</xsl:text>
 </OMI>
 </xsl:when>
 <xsl:when test="count(*) != 0 ">
 <OMI><xsl:copy-of select="@*"/>
  <xsl:text>
  OMI should only have character data</xsl:text>
 </OMI>
 </xsl:when>
 </xsl:choose>
</xsl:template>

<xsl:template match="OMF">
 <xsl:choose>
 <xsl:when test="not((@dec or @hex) and count(@*)=1)">
 <OMF><xsl:copy-of select="@*"/>
  <xsl:text>
  OMF must only have dec or hex attribute</xsl:text>
 </OMF>
 </xsl:when>
 <xsl:when test="count(*) != 0 or not(. = '')">
 <OMF><xsl:copy-of select="@*"/>
  <xsl:text>
  OMF must be EMPTY</xsl:text>
 </OMF>
 </xsl:when>
 </xsl:choose>
</xsl:template>

<xsl:template match="OMA|OMOBJ">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OMA><xsl:copy-of select="@*"/>
  <xsl:text>
  OMA does not take attributes</xsl:text>
 </OMA>
 </xsl:when>
 <xsl:when test="count(*)= 0">
 <OMA>
  <xsl:text>
  OMA must have at least one child element</xsl:text>
 </OMA>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates/>
</xsl:template>


<xsl:template match="OME">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OME><xsl:copy-of select="@*"/>
  <xsl:text>
  OME does not take attributes</xsl:text>
 </OME>
 </xsl:when>
 <xsl:when test="not(*[1][self::OMS])">
 <OME>
  <xsl:text>
  OME must have first child an OMS</xsl:text>
 </OME>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates/>
</xsl:template>



<xsl:template match="OMBIND">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OMBIND><xsl:copy-of select="@*"/>
  <xsl:text>
  OMBIND does not take attributes</xsl:text>
 </OMBIND>
 </xsl:when>
 <xsl:when test="not(count(*)=3 and *[2][self::OMBVAR])">
 <OMBIND>
  <xsl:text>
  OMBIND must have three children, with second child an OMBVAR</xsl:text>
 </OMBIND>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates/>
</xsl:template>




<xsl:template match="OMBVAR">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OMBVAR><xsl:copy-of select="@*"/>
  <xsl:text>
  OMBVAR does not take attributes</xsl:text>
 </OMBVAR>
 </xsl:when>
 <xsl:when test="descendant::*[not(self::OMV or self::OMATTR)]">
  <xsl:text>
  OMBVAR may only contain attributed variables</xsl:text>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates />
</xsl:template>


<xsl:template match="OMATP">
 <xsl:choose>
 <xsl:when test="not(count(@*)=0)">
 <OMATP><xsl:copy-of select="@*"/>
  <xsl:text>
  OMATP does not take attributes</xsl:text>
 </OMATP>
 </xsl:when>
 <xsl:when test="not(*[position() mod 2 =1 and self::OMS])">
 <OMATP>
  <xsl:text>
  OMATP must have odd children OMS</xsl:text>
 </OMATP>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="OMATTR">
 <xsl:choose>
 <xsl:when test="not(count(@*)!=2)">
 <OMATTR><xsl:copy-of select="@*"/>
  <xsl:text>
  OMATTR must have exactly two children</xsl:text>
 </OMATTR>
 </xsl:when>
 <xsl:when test="*[1][not(self::OMATP)]">
  <xsl:text>
  First child of OMATTR must be OMATP</xsl:text>
 </xsl:when>
 </xsl:choose>
 <xsl:apply-templates />
</xsl:template>

</xsl:stylesheet>
