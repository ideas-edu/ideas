<xsl:stylesheet 
  version="1.0"
  xmlns:cdg="http://www.openmath.org/OpenMathCDG"
  xmlns:cd="http://www.openmath.org/OpenMathCD"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
   xmlns:x="http://xml.apache.org/xalan"
   xmlns:xx="http://icl.com/saxon"
  xmlns:exslt='http://exslt.org/common'
>
<xsl:import href="./navigation.xsl"/>
<xsl:output method="html" indent="yes"/>

<xsl:variable name="extra" select="document('cdgroups.xml',.)"/>
<xsl:variable name="contrib" select="document('../cdfiles2/contrib/log.xml',.)"/>


<xsl:template match="/">
<html>
<head>
<title>Index</title>
</head>
<body>

<xsl:call-template name="navigation"/>

<h1>OpenMath Content Dictionaries</h1>

<a href="index.html">CD Home</a>

<hr/>

<xsl:variable name="all">
<xsl:for-each select="$contrib/log/cd[not(@name=following-sibling::cd/@name)]">
<CD path="/cd/contrib" name="{@name}" status="experimental (contributed)"/>
</xsl:for-each>
<xsl:for-each select="$extra/page/ul/li[@id]">
<xsl:for-each select="document(concat('cdgroups/',@id,'.cdg'),.)/cdg:CDGroup/cdg:CDGroupMember/cdg:CDName">
<CD path="." name="{.}" status="{normalize-space(document(concat('../cd/',.,'.ocd'),.)/cd:CD/cd:CDStatus)}"/>
</xsl:for-each>
</xsl:for-each>
</xsl:variable>


<table border="1">
<tr>
<th><a href="cdnames.html">Content Dictionary</a></th>
<th><a href="cdnamess.html">Status</a></th>
<th>XHTML Presentation</th>
<th>XML CD File</th>
<th>STS: XHTML Presentation</th>
<th>XML STS File</th>
</tr>
<xsl:for-each select="exslt:node-set($all)/CD[generate-id()=generate-id(key('c',concat(@path,'/',@name)))]">
<xsl:sort select="@name"/>
<xsl:variable name="hasSts"><xsl:call-template name="test-file-exists">
  <xsl:with-param name="name">xml/cdfiles2/sts/<xsl:value-of select="@name"/>.sts</xsl:with-param>
  </xsl:call-template></xsl:variable>
<xsl:variable name="hasContribSts"><xsl:call-template name="test-file-exists">
  <xsl:with-param name="name">xml/cdfiles2/contrib/sts/<xsl:value-of select="@name"/>.sts</xsl:with-param>
  </xsl:call-template></xsl:variable>
<tr>
<th style="text-align:left"><xsl:value-of select="@name"/></th>
<td><xsl:value-of select="@status"/></td>
<td><a href="{@path}/cd/{@name}.xhtml"><xsl:value-of select="@name"/>.xhtml</a></td>
<td><a href="{@path}/cd/{@name}.ocd"><xsl:value-of select="@name"/>.ocd</a></td>
<xsl:choose>
<xsl:when test="$hasSts = 'true' or $hasContribSts = 'true'">
<td><a href="{@path}/sts/{@name}.xhtml"><xsl:value-of select="@name"/>.xhtml</a></td>
<td><a href="{@path}/sts/{@name}.sts"><xsl:value-of select="@name"/>.sts</a></td>
</xsl:when><xsl:otherwise>
<td><xsl:text> </xsl:text></td><td><xsl:text> </xsl:text></td>
</xsl:otherwise></xsl:choose>
</tr>
</xsl:for-each>
</table>
   <xsl:call-template name="navigation"/>

</body>
</html>
</xsl:template>

<xsl:key name="c" match="CD" use="concat(@path,'/',@name)"/>



<xsl:template name="test-file-exists" xmlns:file="java.io.File">
 <xsl:param name="name" select="'x'"/>
 <xsl:value-of select="file:exists(file:new(string($name)))"/>
</xsl:template>

</xsl:stylesheet>

