<xsl:stylesheet 
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:import href="../../stylesheets/html/om-page.xsl"/>

<xsl:output method="html" indent="yes"/>
<xsl:param name="logxmlfile"/>

<xsl:template match="cdgroups">
<div>
<xsl:apply-templates/>
</div>
</xsl:template>

<xsl:template match="cdgroup">
<a href="cdgroups/{@name}.html"><xsl:apply-templates/></a><br/>
</xsl:template>


<xsl:template match="cdlog">
<dl>
<xsl:for-each select="document($logxmlfile,.)/log/cd[not(@name=following-sibling::cd/@name)]">
<xsl:sort select="@name"/>
<dt><a href="cd/contrib/cd/{@name}.xhtml"><xsl:value-of select="@name"/></a></dt>
<dd>
<div>Author: <xsl:copy-of select="author"/></div>
<div>Submitted: <xsl:copy-of select="date"/></div>
<div><xsl:copy-of select="description/node()"/></div>
</dd>
</xsl:for-each>
</dl>
 
</xsl:template>




</xsl:stylesheet>

