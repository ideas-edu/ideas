<xsl:stylesheet 
  version="1.0"
  xmlns:cdg="http://www.openmath.org/OpenMathCDG"
  xmlns:cd="http://www.openmath.org/OpenMathCD"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:x="http://xml.apache.org/xalan"
   xmlns:xx="http://icl.com/saxon"
  xmlns:exslt='http://exslt.org/common'
>

<xsl:output method="html" indent="yes"/>

<xsl:variable name="extra" select="document('cdgroups.xml',.)"/>
<xsl:variable name="contrib" select="document('../cdfiles2/contrib/log.xml',.)"/>

<xsl:key name="oms" match="cdg:CDDefinition|CDDefinition" use="concat(../@name,':',@name)"/>
<xsl:template match="/">
<html>
<head>
<title>Index</title>
</head>
<body>




<h1>OpenMath Symbols</h1>

<a href="index.html">CD Home</a>

<hr/>

<xsl:variable name="all">
<xsl:for-each select="$contrib/log/cd[not(@name=following-sibling::cd/@name)]">
<CD path="../cdfiles2/contrib" name="{@name}">
<xsl:for-each select="document(concat('cd/',@name,'.ocd'),.)/CD/CDDefinition">
<CDDefinition name="{normalize-space(Name)}">
<xsl:copy-of select="Description"/>
</CDDefinition>
</xsl:for-each>
</CD>
</xsl:for-each>
<xsl:for-each select="$extra/page/ul/li[@id]">
<xsl:for-each select="document(concat('cdgroups/',@id,'.cdg'),.)/cdg:CDGroup/cdg:CDGroupMember/cdg:CDName">
<CD path="." name="{.}">
<xsl:for-each select="document(concat('../cd/',.,'.ocd'),.)/cd:CD/cd:CDDefinition">
<CDDefinition name="{normalize-space(cd:Name)}">
<xsl:copy-of select="cd:Description"/>
</CDDefinition>
</xsl:for-each>
</CD>
</xsl:for-each>
</xsl:for-each>
</xsl:variable>


<table border="1">
<tr>
<th>Symbol</th>
<th>CD</th>
<th>Description</th>
</tr>
<xsl:for-each select="exslt:node-set($all)/CD/CDDefinition">
<xsl:sort select="@name"/>
<xsl:if test="generate-id(.)=generate-id(key('oms',concat(../@name,':',@name)))">
<tr>
<td><a href="{../@path}/cd/{../@name}.xhtml#{@name}"><xsl:value-of select="@name"/></a></td>
<td><xsl:value-of select="../@name"/></td>
<td><xsl:copy-of select="(Description|cd:Description)/node()"/></td>
</tr>
</xsl:if>
</xsl:for-each>
</table>
</body>
</html>
</xsl:template>





</xsl:stylesheet>

