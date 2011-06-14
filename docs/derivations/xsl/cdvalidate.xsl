
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version="1.0"
                >

<xsl:import href="omvalidate.xsl"/>

<xsl:output method="xml" indent="yes" omit-xml-declaration="yes"/>
<xsl:strip-space elements="*"/>

<xsl:param name="cdhome" 
           select="'http://openmath.nag.co.uk/openmath/cd/internal/cd/'"/>


<xsl:template match="/">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="CD">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="CD/CDURL[not(*)]"/>
<xsl:template match="CD/Description[not(*)]"/>
<xsl:template match="CD/CDComment[not(*)]"/>
<xsl:template match="CDDefinition/CDComment[not(*)]"/>

<xsl:template match="CD/CDReviewDate[not(*)]">
  <xsl:if test="translate(.,' &#10;0123456789-','')!=''">
  Bad CD Review Date: (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>

<xsl:template match="CD/CDDate[not(*)]">
  <xsl:if test="translate(.,' &#10;0123456789-','')!=''">
  Bad CD Datee: (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>


<xsl:template match="CD/CDDefinition">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="CDDefinition/Description">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="text()">
 Bad text in <xsl:value-of select="name(..)"/>  element: 
 <xsl:value-of select="."/>
</xsl:template>


<xsl:template match="CD/CDName">
 <xsl:if test="translate(.,' &#10;abcdefghijklmnopqrstuvwxyz0123456789_','')!=''">
  Bad CDName: (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>


<xsl:template match="CDUses/CDName">
 <xsl:variable name="x" select="normalize-space(.)"/>
 <xsl:if test="not(//OMS[@cd=$x])">
  Unused CD <xsl:value-of select="."/> in CDUses list.
 </xsl:if>
</xsl:template>

<xsl:template match="CD/CDVersion|CD/CDRevision">
 <xsl:if test="translate(.,' &#10;0123456789','')!=''">
  Bad CDVersion: (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>

<xsl:template match="CD/CDStatus">
 <xsl:variable name="x" select="normalize-space(.)"/>
 <xsl:if test="$x != 'public' and $x != 'experimental' and $x !='private'">
   Bad status:  (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>


<xsl:template match="CD/CDUses">
 <xsl:apply-templates/>
</xsl:template>



<xsl:template match="CDDefinition/Name">
 <xsl:if test="translate(.,
   ' &#10;&#9;abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_',
   '')!=''">
  Bad Symbol Name: (<xsl:value-of select="."/>)
 </xsl:if>
</xsl:template>


<xsl:template match="CDDefinition/CMP[not(*)]">
</xsl:template>
<xsl:template match="CDDefinition/Description">
   <xsl:apply-templates/>
</xsl:template>

<xsl:template match="CDDefinition/FMP">
  <xsl:apply-templates/>
</xsl:template>
<xsl:template match="CDDefinition/Example">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="Example/text()"/>
<xsl:template match="Description/text()"/>
<xsl:template match="CDUrl/text()"/>



<xsl:template match="OMOBJ">
  <xsl:apply-imports/>
</xsl:template>

<xsl:key name="cduses" match="/CD/CDUses/CDName|/CD/CDName" use="normalize-space(.)"/>

<xsl:template match="OMS">
 <xsl:if test="not(key('cduses', @cd))">
   CD <xsl:value-of select="@cd"/> not listed in CDUses.
  <CDName><xsl:value-of select="@cd"/></CDName>
 </xsl:if>
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

</xsl:stylesheet>