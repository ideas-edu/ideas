<xsl:stylesheet version="2.0" 
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:om="http://www.openmath.org/OpenMath"
		xmlns="http://www.w3.org/1998/Math/MathML"
		exclude-result-prefixes="om"
>

  <xsl:template match="*" mode="om2cmml">
    <xsl:message>om2cmml: <xsl:value-of select="name()"/></xsl:message>
  </xsl:template>

<!--
  <xsl:template match="om:OMATP/*" mode="om2cmml">
    <xsl:message>om2cmml: OMATP/* <xsl:value-of select="name()"/></xsl:message>
    <xsl:apply-templates  mode="om2cmml"/>>
  </xsl:template>
-->

  <xsl:template match="om:OMOBJ" mode="om2cmml">
    <math>
      <xsl:apply-templates mode="om2cmml"/>
    </math>
  </xsl:template>



  <xsl:template match="om:OMS" mode="om2cmml">
    <csymbol cd="{@cd}">
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="@name"/>
    </csymbol>
  </xsl:template>

  <xsl:template match="om:OMV" mode="om2cmml">
    <ci>
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="@name"/>
    </ci>
  </xsl:template>

  <xsl:template match="om:OMSTR" mode="om2cmml">
    <cs>
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="."/>
    </cs>
  </xsl:template>

  <xsl:template match="om:OMB" mode="om2cmml">
    <cbytes>
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="."/>
    </cbytes>
  </xsl:template>



  <xsl:template match="om:OMI" mode="om2cmml">
    <cn>
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="."/>
    </cn>
  </xsl:template>

  <xsl:template match="om:OMBIND" mode="om2cmml">
    <bind>
      <xsl:copy-of select="@id"/>
      <xsl:apply-templates mode="om2cmml"/>
    </bind>
  </xsl:template>


  <xsl:template match="om:OMBVAR" mode="om2cmml">
    <xsl:for-each select="*">
    <bvar>
      <xsl:apply-templates mode="om2cmml" select="."/>
    </bvar>
    </xsl:for-each>
  </xsl:template>


  <xsl:template match="om:OMA" mode="om2cmml">
    <apply>
      <xsl:copy-of select="@id"/>
      <xsl:apply-templates mode="om2cmml"/>
    </apply>
  </xsl:template>

  <xsl:template match="om:OME" mode="om2cmml">
    <cerror>
      <xsl:copy-of select="@id"/>
      <xsl:apply-templates mode="om2cmml"/>
    </cerror>
  </xsl:template>


  <xsl:template match="om:OMATTR" mode="om2cmml">
    <semantics>
      <xsl:apply-templates select="*[last()]" mode="om2cmml"/>
      <xsl:for-each select="om:OMATP/*[position() mod 2 = 1]">
	<annotation-xml cd="{@cd}"  encoding="{@name}">
	  <xsl:apply-templates select="following-sibling::*[1]" mode="om2cmml"/>
	</annotation-xml>
      </xsl:for-each>
    </semantics>
  </xsl:template>

  
  <xsl:template match="om:OMF[@dec]" mode="om2cmml">
    <cn type="real">
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="@dec"/>
    </cn>
  </xsl:template>

  <xsl:template match="om:OMF[@hex]" mode="om2cmml">
    <cn type="ieee-hex">
      <xsl:copy-of select="@id"/>
      <xsl:value-of select="@hex"/>
    </cn>
  </xsl:template>

  <xsl:template match="om:OMR" mode="om2cmml">
    <share href="{@href}"/>
  </xsl:template>

<xsl:template match="om:OMFOREIGN"  mode="om2cmml">
  <xsl:copy-of select="*"/>
</xsl:template>

</xsl:stylesheet>
