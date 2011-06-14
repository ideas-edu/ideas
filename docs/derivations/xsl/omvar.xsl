

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMV[@name='theta']"  >
   <mi>&#x3B8;</mi>
</xsl:template>

<xsl:template match="om:OMV[@name='lambda']"  >
   <mi>&#x3BB;</mi>
</xsl:template>

<xsl:template match="om:OMV[@name='pi']"  >
   <mi>&#x3C0;</mi>
</xsl:template>





<xsl:template match="om:OMV[@name='Gamma']"  >
   <mi>&#x393;</mi>
</xsl:template>


<xsl:template
match="om:OMV[translate(substring(@name,string-length(@name)),'0123456789','0000000000')='0']"
>
<xsl:choose>
 <xsl:when test="substring(@name,string-length(@name)-2)=number(substring(@name,string-length(@name)-2))">
   <msub>
    <mi><xsl:value-of select="substring(@name,1,string-length(@name)-3)"/></mi>
    <mn><xsl:value-of select="substring(@name,string-length(@name)-2)"/></mn>
  </msub>
 </xsl:when>
 <xsl:when test="substring(@name,string-length(@name)-1)=number(substring(@name,string-length(@name)-1))">
   <msub>
    <mi><xsl:value-of select="substring(@name,1,string-length(@name)-2)"/></mi>
    <mn><xsl:value-of select="substring(@name,string-length(@name)-1)"/></mn>
  </msub>
 </xsl:when>
 <xsl:when test="substring(@name,string-length(@name))=number(substring(@name,string-length(@name)))">
   <msub>
    <mi><xsl:value-of select="substring(@name,1,string-length(@name)-1)"/></mi>
    <mn><xsl:value-of select="substring(@name,string-length(@name))"/></mn>
  </msub>
 </xsl:when>
<xsl:otherwise>
  <xsl:message>Confused over OMV [<xsl:value-of select="@name"/>]</xsl:message>
</xsl:otherwise>
</xsl:choose>
</xsl:template>

</xsl:stylesheet>



