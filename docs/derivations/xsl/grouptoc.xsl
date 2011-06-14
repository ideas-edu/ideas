<xsl:stylesheet 
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" indent="yes"/>


<xsl:template match="CDGroup">
  <html>
  <head>
  <title><xsl:value-of select="CDGroupName"/></title>
  </head>
  <body BGCOLOR="#FFFFFF">
  <font size="-1">
  <B>OpenMath CD Group<br/>
  <a target="cdframe" href="../../cdgroups/{normalize-space(CDGroupName)}.cdg">
      <xsl:value-of select="CDGroupName"/>
  </a></B><br/>
  <b>Version: </b><xsl:value-of select="CDGroupVersion"/>
  </font>
  <hr/>
  <font size="-1">
  <xsl:value-of select="CDGroupDescription"/>
  </font>
  <hr/>
  <font size="-1">
  <xsl:apply-templates/>
  </font>
  </body>
  </html>
</xsl:template>

<xsl:template match="CDComment">
</xsl:template>


<xsl:template match="CDGroupDescription">
<!--
<hr/>
<font size="-1">
<xsl:apply-templates/>
</font>
<hr/>
-->
</xsl:template>


<xsl:template match="CDGroupURL|CDGroupName">
</xsl:template>


<xsl:template match="CDGroupVersion"/>
<xsl:template match="CDGroupRevision"/>


<xsl:template match="CDGroupMember">
    <xsl:element name="a">
      <xsl:attribute name="href">cd/<xsl:value-of 
  select="normalize-space(./CDName)"/>.html</xsl:attribute>
      <xsl:attribute name="target">cdframe</xsl:attribute>
      <xsl:value-of select="CDGroupName"/>
<xsl:value-of 
  select="normalize-space(./CDName)"/>
      </xsl:element>
<xsl:value-of  select="normalize-space(./CDGroupMember)"/>
<br/>
</xsl:template>

<xsl:template match="CDURL|CDName">
</xsl:template>

</xsl:stylesheet>

