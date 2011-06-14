<xsl:stylesheet 
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns:cd="http://www.openmath.org/OpenMathCDG"
  xmlns="http://www.w3.org/1999/xhtml">


<xsl:output method="html" indent="yes"/>

<xsl:template name="navigation">
  <hr/>
  <table width="100%" border="0" cellspacing="0" cellpadding="5" title ="Site Navigation Links" summary="Site Navigation Links">
    <tr bgcolor="#FFCC33">
     <b>
       <td align="center"><a href="/index.html">Home</a></td> 
       <td align="center"><a href="/overview/index.html">Overview</a></td> 
       <td align="center"><a href="/documents/index.html">Documents</a></td> 
       <td align="center"><a href="/cd/index.html">Content Dictionaries</a></td> 
       <td align="center"><a href="/software/index.html">Software &amp; Tools</a></td> 
       <td align="center"><a href="/society/index.html">The OpenMath Society</a></td> 
       <td align="center"><a href="/projects/index.html">OpenMath Projects</a></td> 
       <td align="center"><a href="/lists/index.html">OpenMath Discussion Lists</a></td> 
       <td align="center"><a href="/meetings/index.html">OpenMath Meetings</a></td> 
       <td align="center"><a href="/links.html">Links</a></td> 
     </b>
   </tr>
  </table>
  <hr/>
</xsl:template>


<xsl:template match="cd:CDGroup">
  <html>
  <head>
  <title><xsl:value-of select="cd:CDGroupName"/></title>
  </head>
  <body bgcolor="#FFFFFF">
   <xsl:call-template name="navigation"/>
  <h1>OpenMath CD Group:
      <xsl:value-of select="cd:CDGroupName"/>
  </h1>
  <b>Version: </b><xsl:value-of select="cd:CDGroupVersion"/>
  <hr/>
  <xsl:value-of select="cd:CDGroupDescription"/>
  <hr/>
  <xsl:apply-templates/>
   <xsl:call-template name="navigation"/>
  </body>
  </html>
</xsl:template>

<xsl:template match="cd:CDComment">
</xsl:template>



<xsl:template match="cd:CDGroupURL|cd:CDGroupName">
</xsl:template>


<xsl:template match="cd:CDGroupDescription"/>
<xsl:template match="cd:CDGroupVersion"/>
<xsl:template match="cd:CDGroupRevision"/>


<xsl:template match="cd:CDGroupMember">
    <xsl:element name="a">
      <xsl:attribute name="href">../cd/<xsl:value-of 
  select="normalize-space(./cd:CDName)"/>.xhtml</xsl:attribute>
      <xsl:value-of select="cd:CDGroupName"/>
<xsl:value-of 
  select="normalize-space(./cd:CDName)"/>
      </xsl:element>
<xsl:value-of  select="normalize-space(./cd:CDGroupMember)"/>
<br/>
</xsl:template>

<xsl:template match="cd:CDURL|cd:CDName">
</xsl:template>

</xsl:stylesheet>

