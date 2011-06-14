
<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>




<xsl:template match="om:OMS[@cd='interval1' and @name='integer_interval']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>[</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>]</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='interval1' and @name='interval']" >
   <xsl:call-template name="fenced"/>
</xsl:template>



<xsl:template match="om:OMS[@cd='interval1' and @name='interval_oo']" >
   <xsl:call-template name="fenced"/>
</xsl:template>


<xsl:template match="om:OMS[@cd='interval1' and @name='interval_cc']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>[</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>]</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>



<xsl:template match="om:OMS[@cd='interval1' and @name='interval_oc']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>(</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>]</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>

<xsl:template match="om:OMS[@cd='interval1' and @name='interval_co']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>[</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>)</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>


<xsl:template match="om:OMS[@cd='interval1' and @name='ordered_interval']" >
   <xsl:call-template name="fenced">
   <xsl:with-param name="open"><mo>[</mo></xsl:with-param>
   <xsl:with-param name="close"><mo>]</mo></xsl:with-param>
   </xsl:call-template>
</xsl:template>

</xsl:stylesheet>



