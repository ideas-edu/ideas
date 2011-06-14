

<xsl:stylesheet 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:om="http://www.openmath.org/OpenMath"
  xmlns="http://www.w3.org/1998/Math/MathML"
  version="1.0"
>
<xsl:output method="xml" indent="yes"/>

<xsl:include href="omcore.xsl"/>

<xsl:include href="altenc.xsl"/>
<xsl:include href="arith1.xsl"/>
<xsl:include href="arith2.xsl"/>
 <xsl:include href="transc1.xsl"/>
<!-- default OK
 <xsl:include href="minmax1.xsl"/>
-->
<xsl:include href="alg1.xsl"/>
<xsl:include href="complex1.xsl"/>
<xsl:include href="integer1.xsl"/>
<xsl:include href="interval1.xsl"/>
<xsl:include href="nums1.xsl"/>
<xsl:include href="relation1.xsl"/>
<xsl:include href="calculus1.xsl"/>
<xsl:include href="fns1.xsl"/>
<xsl:include href="quant1.xsl"/>
<xsl:include href="logic1.xsl"/>
<xsl:include href="bigfloat1.xsl"/>
<xsl:include href="list1.xsl"/>
<xsl:include href="limit1.xsl"/>
<xsl:include href="linalg1.xsl"/>
<xsl:include href="linalg2.xsl"/>
<xsl:include href="linalg3.xsl"/>
<xsl:include href="linalg5.xsl"/>
<xsl:include href="set1.xsl"/>
<xsl:include href="setname1.xsl"/>
<xsl:include href="setname2.xsl"/>
<xsl:include href="combinat1.xsl"/>
<xsl:include href="piece1.xsl"/>
<!-- -->
<!-- -->
<xsl:include href="fns2.xsl"/>
<xsl:include href="sts.xsl"/>
<xsl:include href="sts2.xsl"/>
<xsl:include href="list2.xsl"/>

<!-- -->
<xsl:include href="omvar.xsl"/>
<!-- -->


<!-- RIACA CDs -->
<xsl:include href="permgp2.xsl"/>
<xsl:include href="set3.xsl"/>
<xsl:include href="integer2.xsl"/>

</xsl:stylesheet>
