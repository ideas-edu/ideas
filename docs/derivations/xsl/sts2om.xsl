
<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
>


<xsl:output
  method="xml"
  indent="yes"
/>

<xsl:strip-space elements="*"/>

<!--
 Convert CDSignaturess to OM objects
 Copyright 1999 David Carlisle
 Version 0.1 1999-04-27
-->

<xsl:template match="/" >
   <OMOBJ>
     <xsl:apply-templates/>
   </OMOBJ>
</xsl:template>

<xsl:template match="*">
  <xsl:param name="cd"/>
   <OMA>
     <OMS cd="metasig" name="{name(.)}"/>
     <xsl:if test="@name">
       <OMS name="{@name}" cd="{$cd}"/>
     </xsl:if>
     <xsl:apply-templates/>
   </OMA>
</xsl:template>


<xsl:template match="text()">
   <OMSTR>
    <xsl:value-of select="normalize-space(.)"/>
   </OMSTR>
</xsl:template>

<xsl:template match="CDSignatures">
  <OMA>
    <OMS cd="metasig" name="CDSignatures"/>
    <OMSTR><xsl:value-of select="@type"/></OMSTR>
    <OMSTR><xsl:value-of select="@cd"/></OMSTR>
    <xsl:apply-templates>
      <xsl:with-param name="cd" select="@cd"/>
    </xsl:apply-templates>
  </OMA>
</xsl:template>


<xsl:template match="OMOBJ">
  <xsl:copy-of select="*"/>
</xsl:template>

</xsl:stylesheet>






