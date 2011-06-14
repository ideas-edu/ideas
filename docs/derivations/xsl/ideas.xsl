<?xml version="1.0" encoding="ISO-8859-1"?> 
<xsl:stylesheet version="1.0"
  xmlns="http://www.w3.org/1999/xhtml"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"> 
  <xsl:strip-space elements="ingredients"/> 
  <xsl:output method="html"/> 
 
<xsl:include href="om2pmml.xsl"/>

<xsl:template match="derivations"> 
  <html> 
   <head><title>Derivations for <xsl:value-of select="@title"/></title> 
   <link rel="STYLESHEET" href="../ideas.css" type="text/css"/>
   </head> 
   <body><div class="content">
   <span class="error">
   Warning: to view this page properly, your browser needs to support
   MathML and XSLT (such as Firefox).</span>
   <h1><xsl:value-of select="@title"/></h1>
   <xsl:apply-templates select="*"/>
   </div></body> 
  </html> 
</xsl:template>
 
<xsl:template match="derivation"> 
<h2><xsl:value-of select="@title"/></h2>
      <div class="derivation">
      	<xsl:apply-templates select="*"/>
      </div>
</xsl:template> 
 
<xsl:template match="step"> 
   <tt><span class="step">
   <xsl:value-of select="."/>
   </span></tt>
</xsl:template> 
 
</xsl:stylesheet>