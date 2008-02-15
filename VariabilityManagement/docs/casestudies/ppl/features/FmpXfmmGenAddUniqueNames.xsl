<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0" 
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    xmlns:fmm="http://www.pnp-software.com/XFeature/fmm"
    xmlns:fm="http://www.pnp-software.com/XFeature/fmm"
    xmlns:dm="http://www.pnp-software.com/XFeature/displayModel" >
    <xsl:output method="xml"/>
    <!-- ========================================================================= -->
    <!-- This matches any node -->
    <xsl:template match="/">
        <xsl:apply-templates/>
    </xsl:template>
    <!-- ========================================================================= -->    
    <xsl:template match="*">
        <xsl:copy>
            <xsl:copy-of select="@*"/>
            <xsl:apply-templates/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="fm:GroupNode">
        <xsl:variable name="gName" select="@fm:value"/>
        <xsl:variable name="num" select="count(preceding::fm:GroupNode[@fm:value=$gName] | ancestor::fm:GroupNode[@fm:value=$gName])"/>
        <xsl:copy>
            <xsl:copy-of select="@*[name() != 'fm:value']"/><!-- not(@fm:value) -->
            <xsl:choose>
                    <xsl:when test="count(//fm:GroupNode[@fm:value=$gName])&gt;1">
                        <xsl:attribute name="fm:value">
                            <xsl:value-of select="concat(@fm:value,'__',$num)"/>
                        </xsl:attribute>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:copy-of select="@fm:value"/>
                    </xsl:otherwise>
                </xsl:choose>
            <xsl:apply-templates/>
        </xsl:copy>
    </xsl:template>

    <xsl:template match="fm:FeatureNode">
        <xsl:variable name="fName" select="@fm:value"/>
        <xsl:variable name="num" select="count(preceding::fm:FeatureNode[@fm:value=$fName] | ancestor::fm:FeatureNode[@fm:value=$fName])"/>
        <xsl:copy>
            <xsl:copy-of select="@*[name() != 'fm:value']"/>
            <xsl:choose>
                    <xsl:when test="count(//fm:FeatureNode[@fm:value=$fName])&gt;1">
                        <xsl:attribute name="fm:value">
                            <xsl:value-of select="concat(@fm:value,'__',$num)"/>
                        </xsl:attribute>
                    </xsl:when>
                    <xsl:otherwise>
                        <xsl:copy-of select="@fm:value"/>
                    </xsl:otherwise>
                </xsl:choose>
            <xsl:apply-templates/>
        </xsl:copy>
    </xsl:template>
    
</xsl:stylesheet>
