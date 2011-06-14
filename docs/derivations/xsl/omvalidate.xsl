
<!-- OpenMath Validating stylesheet version 1.0
     Copyright 1999-2004 David Carlisle, NAG
1999 Initital version (OpenMath 1, no namespace)
2004 2nd version (OpenMath 1 and 2, OpenMath namespace)
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:om="http://www.openmath.org/OpenMath"
                version="1.0"
                   xmlns:saxon="http://icl.com/saxon"
exclude-result-prefixes="saxon om">

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

<xsl:output method="html" indent="yes"/>
<xsl:strip-space elements="*"/>

<xsl:param name="cdhome" 
           select="'/var/tomcat4/webapps/cocoon/openmath/xml/cdfiles/'"/>

<xsl:variable name="om-nons" select="//OMOBJ"/>

<xsl:variable name="om" select="//om:OMOBJ"/>

<xsl:template match="/">
<xsl:variable name="x">
<xsl:if test="$om-nons">
<p>
OpenMath should be in the namespace http://www.openmath.org/OpenMath</p>
</xsl:if>

 <xsl:if test="not($om)">
<p>
No OpenMath found!</p>
</xsl:if>
 <xsl:apply-templates select="$om"/>
</xsl:variable>
<xsl:choose>
<xsl:when test="string($x)">
<xsl:copy-of select="$x"/>
<xsl:message>
<xsl:value-of select="$x"/>
</xsl:message>
</xsl:when>
<xsl:otherwise>
<p>
No OpenMath errors found!</p>
</xsl:otherwise>
</xsl:choose>

</xsl:template>

<xsl:template match="*">
  <p>
Unknown element <xsl:value-of select="name()"/></p>
</xsl:template>

<xsl:template match="om:OMS">
 <xsl:variable name="om2" select="ancestor::om:OMOBJ[1]/@version &gt;= 2.0"/>
 <xsl:if test="not(@name)">
  <p>
OMS must have a name attribute</p>
 </xsl:if>
 <xsl:if test="not(@cd)">
  <p>
OMS must have a cd attribute</p>
 </xsl:if>
 <xsl:choose>
 <xsl:when test="$om2
    and @*[not(name()='name' or name()='cd' or name()='cdbase')]">
  <p>
Bad attribute on OMS:
<xsl:for-each select="@*[not(name()='name' or name()='cd' or name()='cdbase')]">
  <xsl:value-of select="name()"/>="<xsl:value-of select="."/>" 
</xsl:for-each>
</p>
 </xsl:when>
 <xsl:when test="not($om2) and @*[not(name()='name' or name()='cd')]">
  <p>
Bad attribute on OMS:
<xsl:for-each select="@*[not(name()='name' or name()='cd')]">
  <xsl:value-of select="name()"/>="<xsl:value-of select="."/>" 
</xsl:for-each>
</p>
 </xsl:when>
</xsl:choose>
<xsl:if test="$om2 and translate(@cd,'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_','') != ''">
 <p>
Illegal character in CD name: <xsl:value-of select="@cd"/></p>
</xsl:if>
<xsl:if test="$om2 and translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+=(),-./:?!#$&#37;*;@[]^_`{|}','') != ''">
<p>
Illegal character in Symbol name: <xsl:value-of select="@name"/></p>
</xsl:if>

 <xsl:if test="* or not(. = '')">
 <p>
OMS must be EMPTY</p>
 </xsl:if>
<xsl:choose>
<xsl:when test=
  "not(document(concat(@cd,'.ocd'),.)|document(concat($cdhome,'cd/',@cd,'.ocd'))|document(concat($cdhome,'contrib/cd/',@cd,'.ocd')))">
 <p>
Bad Symbol: Unknown CD <xsl:text/>
  <xsl:value-of select="@cd"/>
 </p>
 </xsl:when>
 <xsl:when test=
  "not((document(concat(@cd,'.ocd'),.)|document(concat($cdhome,'cd/',@cd,'.ocd'))|document(concat($cdhome,'contrib/cd/',@cd,'.ocd')))
     /*/*[normalize-space(*[local-name()='Name']) = current()/@name])">
 <p>
Bad Symbol: <xsl:text/>
  <xsl:value-of select="@name"/>
 <xsl:text> not in </xsl:text>
  <xsl:value-of select="@cd"/>
 </p>
 </xsl:when>
 </xsl:choose>
</xsl:template>


<xsl:template match="om:OMV">
 <xsl:variable name="om2" select="ancestor::om:OMOBJ[1]/@version &gt;= 2.0"/>
 <xsl:if test="not(@name and count(@*)=1)">
 <p>
OMV must have just name attribute:
<xsl:for-each select="@*[not(name()='name')]">
  <xsl:value-of select="name()"/>="<xsl:value-of select="."/>" 
</xsl:for-each></p>
 </xsl:if>
 <xsl:if test="* or not(. = '')">
 <p>
OMV must be EMPTY</p>
 </xsl:if>
<xsl:if test="not($om2) and translate(@name,&quot;abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+=(),-\./:?!#$%*;@\[\]\^_`{|}&quot;,'') != ''">
<p>
Illegal character in OMV name="<xsl:value-of select="@name"/>"</p>
 </xsl:if>
</xsl:template>

<xsl:template match="om:OMB|om:OMSTR">
 <xsl:if test="@*">
 <p>
<xsl:value-of select="local-name()"/> does not take attributes</p>
 </xsl:if>
 <xsl:if test="*">
 <p>
<xsl:value-of select="local-name()"/> should only have character data</p>
 </xsl:if>
</xsl:template>

<xsl:template match="om:OMI">
 <xsl:if test="@*">
 <p>
OMI does not take attributes</p>
 </xsl:if>
 <xsl:if test="*">
 <p>
OMI should only have character data</p>
 </xsl:if>
<xsl:if test="translate(.,'-0123456789 ','') != ''">
 <p>
Illegal character in OMI: <xsl:value-of select="."/></p>
</xsl:if>
</xsl:template>

<xsl:template match="om:OMF">
 <xsl:variable name="om2" select="ancestor::om:OMOBJ[1]/@version &gt;= 2.0"/>
 <xsl:if test="not((@dec or @hex) and count(@*)=1)">
 <p>
OMF must only have dec or hex attribute</p>
 </xsl:if>
 <xsl:if test="* or not(. = '')">
 <p>
OMF must be EMPTY</p>
 </xsl:if>
<xsl:if test="not($om2) and translate(@dec,'+-0123456789. ','') != ''">
 <p>
Illegal character in OMF dec attribute: <xsl:value-of select="@dec"/></p>
</xsl:if>
<xsl:if test="$om2 and translate(@dec,'+-0123456789EeINFa. ','') != ''">
 <p>
Illegal character in OMF dec attribute: <xsl:value-of select="@dec"/></p>
</xsl:if>
<xsl:if test="translate(@hex,'ABCDEF0123456789 ','') != ''">
 <p>
Illegal character in OMF hex attribute: <xsl:value-of select="@hex"/></p>
</xsl:if>
</xsl:template>

<xsl:template match="om:OMOBJ">
 <xsl:variable name="om2" select="@version &gt;= 2.0"/>
<xsl:variable name="x">
 <xsl:choose>
 <xsl:when test="$om2 and @*[not(name()='cdbase' or name()='version')]">
  <p>
OMOBJ does not take attributes</p>
 </xsl:when>
 <xsl:when test="not(@version) and  @*">
  <p>
OMOBJ does not take attributes</p>
 </xsl:when>
 </xsl:choose>
 <xsl:if test="not(*)">
  <p>
OMOBJ must have at least one child element</p>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:variable>
<xsl:if test="string($x)">
<p>
Errors in OMOBJ <xsl:value-of select="position()"/>
<xsl:if test="function-available('saxon:line-number')">
on line <xsl:value-of select="saxon:line-number()"/></xsl:if>
<xsl:if test="$om2"> (Validated against OpenMath 2)</xsl:if></p>
<div style="border: solid;margin:1em 1em 1em 1em; padding:1em 1em 1em 1em; ">
<xsl:copy-of select="$x"/>
</div>
</xsl:if>
</xsl:template>

<xsl:template match="om:OMA">
 <xsl:if test="@*[not(name()='id')]">
  <p>
OMA does not take attributes (other than id)</p>
 </xsl:if>
 <xsl:if test="not(*)">
  <p>
OMA must have at least one child element</p>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>


<xsl:template match="om:OME">
 <xsl:if test="@*">
  <p>
OME does not take attributes</p>
 </xsl:if>
 <xsl:if test="not(*[1][self::om:OMS])">
  <p>
OME must have first child an OMS</p>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>



<xsl:template match="om:OMBIND">
 <xsl:if test="@*">
  <p>

  OMBIND does not take attributes</p>
 </xsl:if>
 <xsl:if test="not(count(*)=3 and *[2][self::om:OMBVAR])">
  <p>

  OMBIND must have three children, with second child an OMBVAR</p>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>




<xsl:template match="om:OMBVAR">
 <xsl:if test="@*">
  <p>
OMBVAR does not take attributes</p>
 </xsl:if>
 <xsl:apply-templates mode="var"/>
</xsl:template>

<xsl:template mode="var" match="node()">
 <p>
OMBVAR may only contain variables and attributed variables: <xsl:value-of select="local-name()"/></p>
</xsl:template>

<xsl:template mode="var" match="om:OMV">
 <xsl:apply-templates select="."/>
</xsl:template>

<xsl:template mode="var" match="om:OMATTR">
 <xsl:if test="not(count(*)=2)">
  <p>
OMATTR must have exactly two children</p>
 </xsl:if>
 <xsl:if test="*[1][not(self::om:OMATP)]">
  <p>

  First child of OMATTR must be OMATP</p>
 </xsl:if>
 <xsl:apply-templates select="*[1]"/>
 <xsl:apply-templates mode="var" select="*[2]"/>
</xsl:template>


<xsl:template match="om:OMATP">
 <xsl:if test="@*">
  <p>
OMATP does not take attributes</p>
 </xsl:if>
 <xsl:if test="not(*[position() mod 2 =1 and self::om:OMS])">
  <p>
OMATP must have odd children OMS</p>
 </xsl:if>
  <xsl:if test="not(parent::om:OMATTR)">
   <p>
OMATP must be a child of OMATTR</p>
 </xsl:if>
 <xsl:apply-templates/>
</xsl:template>

<xsl:template match="om:OMATTR">
 <xsl:if test="not(count(*)=2)">
  <p>
OMATTR must have exactly two children</p>
 </xsl:if>
 <xsl:if test="*[1][not(self::om:OMATP)]">
  <p>

  First child of OMATTR must be OMATP</p>
 </xsl:if>
 <xsl:apply-templates />
</xsl:template>

<xsl:template match="om:OMFOREIGN">
 <xsl:variable name="om2" select="ancestor::om:OMOBJ[1]/@version &gt;= 2.0"/>
 <xsl:choose>
 <xsl:when test="not($om2)">
  <p>
OMFOREIGN Not Allowed in Openmath 1</p>
 </xsl:when>
 <xsl:otherwise>
  <xsl:if test="not(parent::om:OMATP)">
   <p>
OMFOREIGN must be a child of OMATP</p>
 </xsl:if>
 <xsl:for-each select="@*[not(name()='encoding')]">
  <p>
Bad attribute on OMFOREIGN: <xsl:value-of select="name()"/>="<xsl:value-of select="."/>"</p>
 </xsl:for-each>
 </xsl:otherwise>
 </xsl:choose>
 <!-- dont check inside omforeign (could check om is correct) -->
</xsl:template>

<xsl:template match="om:OMR">
 <xsl:variable name="om2" select="ancestor::om:OMOBJ[1]/@version &gt;= 2.0"/>
 <xsl:choose>
 <xsl:when test="not($om2)">
  <p>
OMR Not Allowed in Openmath 1</p>
 </xsl:when>
 <xsl:otherwise>
  <xsl:if test="* or not(. = '')">
  <p>
OMR should be EMPTY</p>
 </xsl:if>
 <xsl:if test="not(@href)">
   <p>
OMR must have an href attribute</p>
 </xsl:if>
 </xsl:otherwise>
 </xsl:choose>
</xsl:template>

</xsl:stylesheet>
