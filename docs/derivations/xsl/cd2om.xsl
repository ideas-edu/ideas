
<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.openmath.org/OpenMath"
  xmlns:om="http://www.openmath.org/OpenMath"
  version="1.0"
>

<xsl:output
  method="xml"
  indent="yes"
  omit-xml-declaration="yes"
/>

<xsl:strip-space elements="*"/>

<!--
 Convert CDs to OM objects
 Copyright 1999 David Carlisle
 Version 0.1 1999-02-05
 Version 0.2 1999-04-15
 Version 0.3 1999-04-27
 Version 0.4 2004-07-14 (OM2)
-->

<xsl:template match="/" priority="2" >
   <OMOBJ>
     <xsl:apply-templates/>
   </OMOBJ>
</xsl:template>

<xsl:template match="*" priority="1">
   <OMA>
     <OMS cd="meta" name="{name(.)}"/>
     <xsl:apply-templates/>
   </OMA>
</xsl:template>

<xsl:template match="text()" priority="2">
   <OMSTR>
    <xsl:value-of select="normalize-space(.)"/>
   </OMSTR>
</xsl:template>

<xsl:template match="om:Example|om:FMP" priority="2">
  <OMA>
  <OMS cd="meta" name="{name(.)}"/>
    <xsl:apply-templates mode="om"/>
  </OMA>
</xsl:template>

<xsl:template match="om:Example/text()|om:FMP/text()" priority="2" mode="om">
   <OMSTR>
    <xsl:value-of select="."/>
   </OMSTR>
</xsl:template>


<xsl:template match="om:OMOBJ"  priority="2">
  <xsl:copy-of select="*"/>
</xsl:template>

</xsl:stylesheet>
