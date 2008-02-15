<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" exclude-result-prefixes="xsi">
	<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>

  <!-- ==================================================================== -->
  <xsl:template match="/">
    <xsl:text disable-output-escaping="yes">
&lt;xsl:stylesheet version=&quot;2.0&quot; xmlns:xsl=&quot;http://www.w3.org/1999/XSL/Transform&quot;
    xmlns:fo=&quot;http://www.w3.org/1999/XSL/Format&quot;
    xmlns:fmm=&quot;http://www.pnp-software.com/XFeature/fmm&quot;
    xmlns:fm=&quot;http://www.pnp-software.com/XFeature/fmm&quot;
    xmlns:dm=&quot;http://www.pnp-software.com/XFeature/displayModel&quot;
    xmlns:xs=&quot;http://www.w3.org/2001/XMLSchema&quot; exclude-result-prefixes=&quot;xsl fo fmm fm dm xs&quot;&gt;
    &lt;!-- ========================================================================= --&gt;
    &lt;xsl:template match=&quot;/&quot;&gt;
        &lt;DisplayModel xmlns:xsi=&quot;http://www.w3.org/2001/XMLSchema-instance&quot;
            xsi:noNamespaceSchemaLocation=&quot;DisplayMetamodel.dmm&quot; name=&quot;generated.xdm&quot;&gt;
            &lt;xsl:apply-templates/&gt;
        &lt;/DisplayModel&gt;
    &lt;/xsl:template&gt;
    &lt;!-- ========================================================================= --&gt;
    </xsl:text>
    <xsl:apply-templates/>
    
    
    <xsl:text disable-output-escaping="yes">    
&lt;/xsl:stylesheet&gt;
    </xsl:text>    
  </xsl:template>
  <!-- ==================================================================== -->

    <xsl:template match="*" mode="copy">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates mode="copy"/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="DisplayString" mode="copy">
            <xsl:element name="DisplayString">
                <xsl:attribute name="value">
                    <xsl:text>{$nm}</xsl:text>
                </xsl:attribute>
                <xsl:apply-templates mode="copy"/>
            </xsl:element>
    </xsl:template>
    
  <!-- ==================================================================== -->
  <xsl:template match="//NodeDisplayFormat[@value='FeatureModel']">
    <xsl:text disable-output-escaping="yes">
    &lt;!-- ================================================================ --&gt;
    &lt;xsl:template match=&quot;xs:element[xs:complexType/@dm:displayType=&apos;root&apos;]&quot;&gt;
        &lt;xsl:variable name=&quot;nm&quot; select=&quot;@name&quot;/&gt;
    </xsl:text>     
            <xsl:element name="NodeDisplayFormat">
                <xsl:attribute name="value">
                    <xsl:text>{$nm}</xsl:text>
                </xsl:attribute>

                <xsl:apply-templates mode="copy"/>
            </xsl:element>
    <xsl:text disable-output-escaping="yes">    
    &lt;/xsl:template&gt;
    </xsl:text>          
  </xsl:template>
  <!-- ==================================================================== -->    
    
  <!-- ==================================================================== -->
  <xsl:template match="//NodeDisplayFormat[@value='RootFeature']">
    <xsl:text disable-output-escaping="yes">
    &lt;!-- ================================================================ --&gt;
    &lt;xsl:template match=&quot;xs:element[xs:complexType/@dm:displayType=&apos;node&apos;]&quot;&gt;
        &lt;xsl:variable name=&quot;nm&quot; select=&quot;@name&quot;/&gt;
    </xsl:text>     
            <xsl:element name="NodeDisplayFormat">
                <xsl:attribute name="value">
                    <xsl:text>{$nm}</xsl:text>
                </xsl:attribute>

                <xsl:apply-templates mode="copy"/>
            </xsl:element>
    <xsl:text disable-output-escaping="yes">    
    &lt;/xsl:template&gt;
    </xsl:text>          
  </xsl:template>
  <!-- ==================================================================== -->




  <!-- ==================================================================== -->
  <xsl:template match="//PropertySetDisplayFormat[@value='IntegerProperties']">
    <xsl:text disable-output-escaping="yes">
    &lt;!-- ================================================================ --&gt;
    &lt;xsl:template match=&quot;xs:element[xs:complexType/@dm:displayType=&apos;propertySet&apos;]&quot;&gt;
        &lt;xsl:variable name=&quot;nm&quot; select=&quot;@name&quot;/&gt;
    </xsl:text>     
            <xsl:element name="PropertySetDisplayFormat">
                <xsl:attribute name="value">
                    <xsl:text>{$nm}</xsl:text>
                </xsl:attribute>

                <xsl:apply-templates mode="copy"/>
            </xsl:element>
    <xsl:text disable-output-escaping="yes">    
    &lt;/xsl:template&gt;
    </xsl:text>          
  </xsl:template>
  <!-- ==================================================================== -->




  <!-- ==================================================================== -->
  <xsl:template match="//PropertyDisplayFormat[@value='IntegerValue']">
    <xsl:text disable-output-escaping="yes">
    &lt;!-- ================================================================ --&gt;
    &lt;xsl:template match=&quot;xs:element[xs:complexType/@dm:displayType=&apos;property&apos;]&quot;&gt;
        &lt;xsl:variable name=&quot;nm&quot; select=&quot;@name&quot;/&gt;
    </xsl:text>     
            <xsl:element name="PropertyDisplayFormat">
                <xsl:attribute name="value">
                    <xsl:text>{$nm}</xsl:text>
                </xsl:attribute>

                <xsl:apply-templates mode="copy"/>
            </xsl:element>
    <xsl:text disable-output-escaping="yes">    
    &lt;/xsl:template&gt;
    </xsl:text>          
  </xsl:template>
  <!-- ==================================================================== -->
  
  
</xsl:stylesheet>
