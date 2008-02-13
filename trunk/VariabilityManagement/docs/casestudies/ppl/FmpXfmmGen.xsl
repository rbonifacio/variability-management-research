<?xml version="1.0" encoding="UTF-8"?>
<!-- The task of this stylesheet is to transform a framework description made in XML into applicaction XML schema. -->
<!-- by Ondrej Rohlik (Automatic Control Laboratory, ETH Zuerich) -->
<!--                                                              -->
<!-- ToDo: - For now ignoring the GroupedReference                -->
<!--                                                              -->
<!--                                                              -->
<!--                                                              -->
<!--                                                              -->

<xsl:stylesheet version="2.0" xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fmm="http://www.pnp-software.com/XFeature/fmm"
  xmlns:fm="http://www.pnp-software.com/XFeature/fmm"
  xmlns:dm="http://www.pnp-software.com/XFeature/displayModel"
  xmlns:inst="http://www.pnp-software.com/XFeature/instantiation"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" exclude-result-prefixes="xsi">
  <!-- To clarify: fm is needed for input xfm document and fmm is needed for output document -->
  <xsl:output method="xml"/>
  <!-- ========================================================================= -->
  <!-- The template matches any node. -->
  <xsl:template match="/">
    <xs:schema targetNamespace="http://www.pnp-software.com/XFeature/fmm"
      xmlns:xs="http://www.w3.org/2001/XMLSchema"
      xmlns:dm="http://www.pnp-software.com/XFeature/displayModel"
      xmlns:fmm="http://www.pnp-software.com/XFeature/fmm"
      xmlns:fm="http://www.pnp-software.com/XFeature/fmm"
      xmlns:inst="http://www.pnp-software.com/XFeature/instantiation" 
      elementFormDefault="qualified"
      attributeFormDefault="qualified" 
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
      xsi:schemaLocation="http://www.w3.org/2001/XMLSchema ../../general_files/MetaMetaModel.xfmmm">
      <xs:annotation>
        <xs:appinfo>
          <dm:OxygenToolXPathWorkaround/>
        </xs:appinfo>
      </xs:annotation>
      <xs:simpleType name="zero2inf">
        <xs:restriction base="xs:token">
          <xs:pattern value="([1-9][0-9]*|0|\+|\*)"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="one2inf">
        <xs:restriction base="xs:token">
          <xs:pattern value="([1-9][0-9]*|\+|\*)"/>
        </xs:restriction>
      </xs:simpleType>
      <xs:simpleType name="greaterThanZero">
        <xs:restriction base="xs:token">
          <xs:pattern value="([1-9][0-9]*)"/>
        </xs:restriction>
      </xs:simpleType>
      <xsl:comment> Definition of simple elements </xsl:comment> 
      <xs:attribute name="name" type="xs:string"/>
      <xs:attribute name="type" type="xs:string"/>
      <xs:attribute name="cardMin" type="fmm:zero2inf"/>
      <xs:attribute name="cardMax" type="fmm:one2inf"/>
      <xs:attribute name="value" type="xs:string"/>
      <xs:attribute name="text" type="xs:string"/>
      
      <xs:element name="Annotation">
        <xs:complexType dm:displayType="propertySet">
          <!--<xs:sequence>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xs:element ref="fmm:ShortDescription" minOccurs="0"/>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            <xsl:comment> ======================================= </xsl:comment>
            
          </xs:sequence>-->
          <xs:attribute ref="fmm:value" use="required"/>
        </xs:complexType>
      </xs:element>
      <!--
      <xs:element name="IntegerProperties">
        <xs:complexType dm:displayType="propertySet">
          <xs:sequence>
            <xs:element ref="fmm:IntegerValue" minOccurs="0"/>
            <xs:element ref="fmm:IntegerDefaultValue" minOccurs="0"/>
          </xs:sequence>
          <xs:attribute ref="fmm:value" use="required"/>
        </xs:complexType>
      </xs:element>  
      <xs:element name="StringProperties">
        <xs:complexType dm:displayType="propertySet">
          <xs:sequence>
            <xs:element ref="fmm:StringValue" minOccurs="0"/>
            <xs:element ref="fmm:StringDefaultValue" minOccurs="0"/>
          </xs:sequence>
          <xs:attribute ref="fmm:value" use="required"/>
        </xs:complexType>
      </xs:element>  
      <xs:element name="FloatProperties">
        <xs:complexType dm:displayType="propertySet">
          <xs:sequence>
            <xs:element ref="fmm:FloatValue" minOccurs="0"/>
            <xs:element ref="fmm:FloatDefaultValue" minOccurs="0"/>
          </xs:sequence>
          <xs:attribute ref="fmm:value" use="required"/>
        </xs:complexType>
      </xs:element> 
      -->
      
      
      
      
      
      
      
      
      
      <xsl:apply-templates/>
      <!--  select="//fm:FeatureNode" -->
      <!-- TBDeleted -->
      <!-- <xsl:call-template name="CreateDescriptionElement"/> -->
      <!-- TBDeleted -->
      <!-- ============================ Property Sets ========================= -->
      <!-- COMPONENTS (if GENERABLE) -->
      <xsl:if test="//fm:ComponentPropertySet/fm:StatusProperty/@fm:value='generable'">
        <xs:element name="ComponentInstantiationPropertySet">
          <xs:complexType dm:displayType="propertySet">
            <xs:sequence>
              <xs:element ref="fmm:MissingClassNameProperty"/>
            </xs:sequence>
            <xs:attribute ref="fmm:value" use="required"/>
          </xs:complexType>
        </xs:element>
        <xs:element name="MissingClassNameProperty">
          <xs:complexType dm:displayType="property">
            <xs:attribute name="value" type="xs:string" use="required"/>
          </xs:complexType>
        </xs:element>
      </xsl:if>
      <!-- ATTRIBUTES incl. Enumeration -->
      <xsl:for-each-group select="//fm:Integer" group-by="../../@fm:value">
        <xsl:variable name="featureName" select="../../@fm:value"/>
        <xsl:variable name="featureType" select="xs:integer"/>
        <xs:element name="{$featureName}PropertySet">
          <xs:complexType dm:displayType="propertySet">
            <xs:sequence>
              <xs:element ref="fmm:{$featureName}Property"/>
            </xs:sequence>
            <xs:attribute ref="fmm:value" use="required"/>
          </xs:complexType>
        </xs:element>
        <xs:element name="{$featureName}Property">
          <xs:complexType dm:displayType="property">
            <xsl:choose>
              <xsl:when test="fm:IntegerProperties/fm:IntegerValue/@fm:value">
                <xs:attribute name="value" use="required" type="xs:integer" fixed="{fm:IntegerProperties/fm:IntegerValue/@fm:value}"/>
              </xsl:when>
              <xsl:otherwise>
                <xs:attribute name="value" use="optional" type="xs:integer" default="{fm:IntegerProperties/fm:IntegerDefaultValue/@fm:value}"/>
              </xsl:otherwise>
            </xsl:choose>  
          </xs:complexType>
        </xs:element>
      </xsl:for-each-group>
      <xsl:for-each-group select="//fm:String" group-by="../../@fm:value">
        <xsl:variable name="featureName" select="../../@fm:value"/>
        <xsl:variable name="featureType" select="xs:string"/>
        <xs:element name="{$featureName}PropertySet">
          <xs:complexType dm:displayType="propertySet">
            <xs:sequence>
              <xs:element ref="fmm:{$featureName}Property"/>
            </xs:sequence>
            <xs:attribute ref="fmm:value" use="required"/>
          </xs:complexType>
        </xs:element>
        <xs:element name="{$featureName}Property">
          <xs:complexType dm:displayType="property">
            <xsl:choose>
              <xsl:when test="fm:StringProperties/fm:StringValue/@fm:value">
                <xs:attribute name="value" use="required" type="xs:string" fixed="{fm:StringProperties/fm:StringValue/@fm:value}"/>
              </xsl:when>
              <xsl:otherwise>
                <xs:attribute name="value" use="optional" type="xs:string" default="{fm:StringProperties/fm:StringDefaultValue/@fm:value}"/>
              </xsl:otherwise>
            </xsl:choose>  
          </xs:complexType>
        </xs:element>
      </xsl:for-each-group>
      <xsl:for-each-group select="//fm:Float" group-by="../../@fm:value">
        <xsl:variable name="featureName" select="../../@fm:value"/>
        <xsl:variable name="featureType" select="xs:float"/>
        <xs:element name="{$featureName}PropertySet">
          <xs:complexType dm:displayType="propertySet">
            <xs:sequence>
              <xs:element ref="fmm:{$featureName}Property"/>
            </xs:sequence>
            <xs:attribute ref="fmm:value" use="required"/>
          </xs:complexType>
        </xs:element>
        <xs:element name="{$featureName}Property">
          <xs:complexType dm:displayType="property">
            <xsl:choose>
              <xsl:when test="fm:FloatProperties/fm:FloatValue/@fm:value">
                <xs:attribute name="value" use="required" type="xs:float" fixed="{fm:FloatProperties/fm:FloatValue/@fm:value}"/>
              </xsl:when>
              <xsl:otherwise>
                <xs:attribute name="value" use="optional" type="xs:float" default="{fm:FloatProperties/fm:FloatDefaultValue/@fm:value}"/>
              </xsl:otherwise>
            </xsl:choose>  
          </xs:complexType>
        </xs:element>
      </xsl:for-each-group>
    </xs:schema>
  </xsl:template>
  <!-- end of template for the main xs:schema element -->
  
  
  <!-- ========================================================================= -->
  <!-- The template matches Feature element. -->
  <xsl:template match="/fm:FeatureModel">
    <xs:element name="FeatureModel">
      <xs:complexType dm:displayType="root">
        <xs:choice>
          <xsl:for-each select="/fm:FeatureModel/fm:RootFeature">
            <xs:element ref="fmm:{@fm:value}"/>
          </xsl:for-each>
        </xs:choice>
        <xs:attribute ref="fmm:value" use="required"/>
      </xs:complexType>
    </xs:element>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- The template matches Feature element. -->
  <xsl:template match="//fm:RootFeature">
    <xsl:call-template name="ProcessRootFeature"/>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template match="//fm:SolitaryFeature">
    <xsl:if test="not(fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound')">
      <xsl:if test="fm:Cardinality/@fm:cardMin gt fm:Cardinality/@fm:cardMax">
        <xsl:message terminate="yes">ERROR: min cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMin"/>) has to be less or equal to max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>).</xsl:message>
      </xsl:if>
    </xsl:if>   
    <xsl:call-template name="ProcessSolitaryFeature"/>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template match="//fm:GroupedFeature">
    <xsl:if test="not(fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound')">
      <xsl:if test="fm:Cardinality/@fm:cardMin gt fm:Cardinality/@fm:cardMax">
        <xsl:message terminate="yes">ERROR: min cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMin"/>) has to be less or equal to max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>).</xsl:message>
      </xsl:if>
    </xsl:if>   
    <xsl:call-template name="ProcessSolitaryFeature"/>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- ========================================================================= -->
  <!--<xsl:template match="//fm:GroupedReference">
    <xsl:if test="not(fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound')">
      <xsl:if test="fm:Cardinality/@fm:cardMin gt fm:Cardinality/@fm:cardMax">
        <xsl:message terminate="yes">ERROR: min cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMin"/>) has to be less or equal to max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>).</xsl:message>
      </xsl:if>
    </xsl:if>   
    <xsl:call-template name="ProcessSolitaryFeature"/>
    <xsl:apply-templates/>
  </xsl:template>-->  
  <!-- ========================================================================= -->
  <!--<xsl:template match="//fm:SolitaryReference">
    <xsl:if test="not(fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound')">
      <xsl:if test="fm:Cardinality/@fm:cardMin gt fm:Cardinality/@fm:cardMax">
        <xsl:message terminate="yes">ERROR: min cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMin"/>) has to be less or equal to max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>).</xsl:message>
      </xsl:if>
    </xsl:if>   
    <xsl:call-template name="ProcessSolitaryReference"/>
    <xsl:apply-templates/>
  </xsl:template>-->
  <!-- ========================================================================= -->
  <!-- The template matches Group element -->
  <xsl:template match="//fm:FeatureGroup">
    <!-- create a new group -->
    <xs:group name="{@fm:value}">
      <!-- ========================================================================= -->
      <xsl:if test="not(fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound')">
        <xsl:if test="fm:Cardinality/@fm:cardMin gt fm:Cardinality/@fm:cardMax">
          <xsl:message terminate="yes">ERROR: min cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMin"/>) has to be less or equal to max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>).</xsl:message>
        </xsl:if>
        <xsl:if test="(count(child::fm:GroupedFeature) + count(child::fm:GroupedReference)) &lt; fm:Cardinality/@fm:cardMax">
          <xsl:message terminate="yes">ERROR: there are less child elements (<xsl:value-of select="count(child::fm:GroupedFeature) + count(child::fm:GroupedReference)"/>) of group '<xsl:value-of select="@fm:value"/>' than is the max cardinality (<xsl:value-of select="fm:Cardinality/@fm:cardMax"/>) of the group.</xsl:message>
        </xsl:if>
      </xsl:if>   
      <!-- It processes a group cardinality in case that the minimal cardinality is 1 and maximal 1 -->
      <xsl:if test="(fm:Cardinality/@fm:cardMin=1) and (fm:Cardinality/@fm:cardMax=1)">
        <xsl:if test="count(child::fm:GroupedFeature | child::fm:GroupedReference)=1">
          <xsl:if test="(count(child::fm:GroupedFeature)=1) and (count(child::fm:GroupedReference)=0)">
            <xs:sequence>
              <xsl:for-each select="fm:GroupedFeature">
                <xsl:call-template name="RefFeature"/>
              </xsl:for-each>
            </xs:sequence>
          </xsl:if>
          <xsl:if test="(count(child::fm:GroupedFeature)=0) and (count(child::fm:GroupedReference)=1)">
            <xs:sequence>
              <xsl:for-each select="fm:GroupedReference">
                <xsl:call-template name="RefCallMacro"/>
              </xsl:for-each>
            </xs:sequence>
          </xsl:if>
        </xsl:if>
        <xsl:if test="count(child::fm:GroupedFeature | child::fm:GroupedReference)&gt;1">
          <xs:choice>
            <xsl:for-each select="fm:GroupedFeature">
              <xsl:call-template name="RefFeature"/>
            </xsl:for-each>
            <xsl:for-each select="fm:GroupedReference">
              <xsl:call-template name="RefCallMacro"/>
            </xsl:for-each>
          </xs:choice>
        </xsl:if>
      </xsl:if>
      <!-- ========================================================================= -->
      <!-- It processes a group cardinality in case that the minimal cardinality is 1 and maximal bigger than 1 -->
      <xsl:if test="(fm:Cardinality/@fm:cardMin&gt;=1) and (fm:Cardinality/@fm:cardMax&gt;1)">
        <xs:choice>
          <xsl:call-template name="ExpandGroupCardinality">
            <xsl:with-param name="minCard" select="fm:Cardinality/@fm:cardMin"/>
            <xsl:with-param name="maxCard" select="fm:Cardinality/@fm:cardMax"/>
          </xsl:call-template>
        </xs:choice>
      </xsl:if>
      <!-- ========================================================================= -->
      <!-- It processes a group cardinality in case that the minimal cardinality is 0 and maximal equal or bigger than 1 -->
      <xsl:if test="(fm:Cardinality/@fm:cardMin=0) and (fm:Cardinality/@fm:cardMax&gt;=1)">
        <xs:choice>
          <xsl:call-template name="ExpandGroupCardinality">
            <xsl:with-param name="minCard" select="1"/>
            <xsl:with-param name="maxCard" select="fm:Cardinality/@fm:cardMax"/>
          </xsl:call-template>
        </xs:choice>
      </xsl:if>
    </xs:group>
    <xsl:apply-templates/>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- The template makes all possible combinations of features defined in a group based on group cardinality. -->
  <xsl:template name="ExpandGroupCardinality">
    <!-- store minimal group cardinality -->
    <xsl:param name="minCard"/>
    <!-- store maximal group cardinality -->
    <xsl:param name="maxCard"/>
    <xsl:variable name="minCombinationLength" select="$minCard"/>
    <xsl:variable name="maxCombinationLength" select="$maxCard"/>
    <!-- the current length of the combination -->
    <xsl:variable name="currentLength" select="1"/>
    <!-- The loop makes combinations for each feature or callmacro that is direct child of the group element,
		the length of combinations is driven by minimal and maximal combination length. -->
    <xsl:for-each select="fm:GroupedFeature | fm:GroupedReference">
      <xsl:if test="count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=($minCombinationLength+(-1))">
        <xs:sequence>
          <xsl:if test="self::fm:GroupedFeature">
            <xsl:call-template name="RefFeature"/>
          </xsl:if>
          <xsl:if test="self::fm:GroupedReference">
            <xsl:call-template name="RefCallMacro"/>
          </xsl:if>
          <xsl:if test="count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=1">
            <!-- it calls template where the combination are made by recursion -->
            <xsl:call-template name="MakeCombinations">
              <xsl:with-param name="list" select="following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference"/>
              <xsl:with-param name="minCombinationLength" select="$minCombinationLength"/>
              <xsl:with-param name="maxCombinationLength" select="$maxCombinationLength"/>
              <xsl:with-param name="currentLength" select="$currentLength+1"/>
            </xsl:call-template>
          </xsl:if>
        </xs:sequence>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- The template MakeCombinations is called from ExpandGroupCardinality template. MakeCombination makes the combination by recursion. -->
  <xsl:template name="MakeCombinations">
    <xsl:param name="list"/>
    <xsl:param name="minCombinationLength"/>
    <xsl:param name="maxCombinationLength"/>
    <xsl:param name="currentLength"/>
    <xsl:if test="($minCombinationLength&gt;=$currentLength) and ($maxCombinationLength&gt;$currentLength) and count($list)&gt;=1">
      <xs:choice>
        <xsl:for-each select="$list">
          <xsl:if test="(count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=($maxCombinationLength+(-$currentLength))) or (count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=($minCombinationLength+(-$currentLength)))">
            <xs:sequence>
              <xsl:if test="self::fm:GroupedFeature">
                <xsl:call-template name="RefFeature"/>
              </xsl:if>
              <xsl:if test="self::fm:GroupedReference">
                <xsl:call-template name="RefCallMacro"/>
              </xsl:if>
              <xsl:call-template name="MakeCombinations">
                <xsl:with-param name="list" select="following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference"/>
                <xsl:with-param name="minCombinationLength" select="$minCombinationLength"/>
                <xsl:with-param name="maxCombinationLength" select="$maxCombinationLength"/>
                <xsl:with-param name="currentLength" select="$currentLength+1"/>
              </xsl:call-template>
            </xs:sequence>
          </xsl:if>
        </xsl:for-each>
      </xs:choice>
    </xsl:if>
    <xsl:if test="($minCombinationLength&lt;$currentLength) and ($maxCombinationLength&gt;$currentLength) and count($list)&gt;=1">
      <xs:choice minOccurs="0">
        <xsl:for-each select="$list">
          <xsl:if test="(count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=($maxCombinationLength+(-$currentLength))) or (count(following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference)&gt;=($minCombinationLength+(-$currentLength)))">
            <xs:sequence>
              <xsl:if test="self::fm:GroupedFeature">
                <xsl:call-template name="RefFeature"/>
              </xsl:if>
              <xsl:if test="self::fm:GroupedReference">
                <xsl:call-template name="RefCallMacro"/>
              </xsl:if>
              <xsl:call-template name="MakeCombinations">
                <xsl:with-param name="list" select="following-sibling::fm:GroupedFeature | following-sibling::fm:GroupedReference"/>
                <xsl:with-param name="minCombinationLength" select="$minCombinationLength"/>
                <xsl:with-param name="maxCombinationLength" select="$maxCombinationLength"/>
                <xsl:with-param name="currentLength" select="$currentLength+1"/>
              </xsl:call-template>
            </xs:sequence>
          </xsl:if>
        </xsl:for-each>
      </xs:choice>
    </xsl:if>
    <xsl:if test="($currentLength=$maxCombinationLength) and ($minCombinationLength=$maxCombinationLength) and count($list)&gt;=1">
      <xs:choice>
        <xsl:for-each select="$list">
          <xsl:if test="self::fm:GroupedFeature">
            <xsl:call-template name="RefFeature"/>
          </xsl:if>
          <xsl:if test="self::fm:GroupedReference">
            <xsl:call-template name="RefCallMacro"/>
          </xsl:if>
        </xsl:for-each>
      </xs:choice>
    </xsl:if>
    <xsl:if test="($currentLength=$maxCombinationLength) and ($minCombinationLength&lt;$maxCombinationLength) and count($list)&gt;=1">
      <xs:choice minOccurs="0">
        <xsl:for-each select="$list">
          <xsl:if test="self::fm:GroupedFeature">
            <xsl:call-template name="RefFeature"/>
          </xsl:if>
          <xsl:if test="self::fm:GroupedReference">
            <xsl:call-template name="RefCallMacro"/>
          </xsl:if>
        </xsl:for-each>
      </xs:choice>
    </xsl:if>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- ProcessRootFeature template processes the tag 'RootFeature'. -->
  <xsl:template name="ProcessRootFeature">
    <xs:element name="{@fm:value}">
      <!-- process feature properties, description, group and type -->
      <!-- XXX O.R. next line ommitted for XFeature XXX -->
      <!-- <xsl:call-template name="Description"/> -->
      <xs:complexType dm:displayType="node">
        <!--<xsl:call-template name="Annotation"/>-->
        <xs:sequence>
          <!-- it makes a  reference to globally defined 'Description' element -->
          <!-- XXX O.R. next line ommitted for XFeature XXX -->
          <!-- <xs:element ref="Description" minOccurs="0"/> -->
          <xsl:for-each select="fm:FeatureGroup">
            <xsl:choose>
              <xsl:when test="(fm:GroupCardinality/@fm:cardMin=0) and (fm:GroupCardinality/@fm:cardMax&gt;=1)">
                <xs:group ref="fmm:{@fm:value}" minOccurs="0"/>
              </xsl:when>
              <xsl:otherwise>
                <!-- it makes a  reference to globally defined group element -->
                <xs:group ref="fmm:{@fm:value}"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:for-each select="fm:SolitaryFeature">
            <xsl:variable name="min" select="fm:Cardinality/@fm:cardMin"/>
            <xsl:variable name="max">
              <xsl:choose>
                <xsl:when test="fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound' or fm:Cardinality/@fm:cardMax='unbounded'">
                  <xsl:value-of select="'unbounded'"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>  
            <xsl:choose>
              <xsl:when test="fm:Cardinality">
                <xs:element ref="fmm:{@fm:value}" minOccurs="{$min}" maxOccurs="{$max}"/>
              </xsl:when>
              <xsl:otherwise>
                <xs:element ref="fmm:{@fm:value}" minOccurs="1" maxOccurs="1"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:for-each select="fm:SolitaryReference">
            <xsl:variable name="min" select="fm:Cardinality/@fm:cardMin"/>
            <xsl:variable name="max">
              <xsl:choose>
                <xsl:when test="fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound' or fm:Cardinality/@fm:cardMax='unbounded'">
                  <xsl:value-of select="'unbounded'"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>  
            <xs:element ref="fmm:{@fm:value}" minOccurs="{$min}" maxOccurs="{$max}"/>
          </xsl:for-each>
<!--<xsl:call-template name="PropertySet"/>--><!-- ######################### -->
        </xs:sequence>
        <!-- This calls 'Type' template that is further processing different subtypes. -->
        <!-- XXX to be changed to choice of propertySet -->
        <!-- <xsl:call-template name="PropertySet"/> -->
        <xs:attribute ref="fmm:value" use="required"/>
      </xs:complexType>
    </xs:element>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- ProcessRootFeature template processes the tag 'RootFeature'. -->
  <xsl:template name="ProcessSolitaryFeature">
    <xs:element name="{@fm:value}">
      <!-- process feature properties, description, group and type -->
      <!-- XXX O.R. next line ommitted for XFeature XXX -->
      <!-- <xsl:call-template name="Description"/> -->
      <xs:complexType dm:displayType="node">
<!--<xsl:call-template name="Annotation"/>--><!-- #################### -->
        <xs:sequence>
          <!-- it makes a  reference to globally defined 'Description' element -->
          <!-- XXX O.R. next line ommitted for XFeature XXX -->
          <!-- <xs:element ref="Description" minOccurs="0"/> -->
          <xsl:for-each select="fm:FeatureGroup">
            <xsl:choose>
              <xsl:when test="(fm:Cardinality/@fm:cardMin=0) and (fm:Cardinality/@fm:cardMax&gt;=1)">
                <xs:group ref="fmm:{@fm:value}" minOccurs="0"/>
              </xsl:when>
              <xsl:otherwise>
                <!-- it makes a  reference to globally defined group element -->
                <xs:group ref="fmm:{@fm:value}"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:for-each select="fm:SolitaryFeature">
            <xsl:variable name="min" select="fm:Cardinality/@fm:cardMin"/>
            <xsl:variable name="max">
              <xsl:choose>
                <xsl:when test="fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound' or fm:Cardinality/@fm:cardMax='unbounded'">
                  <xsl:value-of select="'unbounded'"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>  
            <xsl:choose>
              <xsl:when test="fm:Cardinality">
                <xs:element ref="fmm:{@fm:value}" minOccurs="{$min}" maxOccurs="{$max}"/>
              </xsl:when>
              <xsl:otherwise>
                <xs:element ref="fmm:{@fm:value}" minOccurs="1" maxOccurs="1"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:for-each>
          <xsl:for-each select="fm:SolitaryReference">
            <xsl:variable name="min" select="fm:Cardinality/@fm:cardMin"/>
            <xsl:variable name="max">
              <xsl:choose>
                <xsl:when test="fm:Cardinality/@fm:cardMax='+' or fm:Cardinality/@fm:cardMax='*' or fm:Cardinality/@fm:cardMax='unbound' or fm:Cardinality/@fm:cardMax='unbounded'">
                  <xsl:value-of select="'unbounded'"/>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
                </xsl:otherwise>
              </xsl:choose>
            </xsl:variable>  
            <xs:element ref="fmm:{@fm:value}" minOccurs="{$min}" maxOccurs="{$max}"/>
          </xsl:for-each>
        <xsl:call-template name="PropertySet"/>
        </xs:sequence>
        <!-- This calls 'Type' template that is further processing different subtypes. -->
        <!-- XXX to be changed to choice of propertySet -->
        <!-- <xsl:call-template name="PropertySet"/> -->
        <xs:attribute ref="fmm:value" use="required"/>
      </xs:complexType>
    </xs:element>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- ProcessRootFeature template processes the tag 'RootFeature'. -->
  <xsl:template name="ProcessSolitaryReference">
    <xs:element name="{@fm:value}">
      <!-- process feature properties, description, group and type -->
      <!-- XXX O.R. next line ommitted for XFeature XXX -->
      <!-- <xsl:call-template name="Description"/> -->
      <xs:complexType dm:displayType="node">
        <!--<xsl:call-template name="Annotation"/>-->
        <xs:sequence>
          <!-- it makes a  reference to globally defined 'Description' element -->
          <!-- XXX O.R. next line ommitted for XFeature XXX -->
          <!-- <xs:element ref="Description" minOccurs="0"/> -->
<!--<xsl:call-template name="PropertySet"/>--><!-- ######################### -->
        </xs:sequence>
        <!-- This calls 'Type' template that is further processing different subtypes. -->
        <!-- XXX to be changed to choice of propertySet -->
        <!-- <xsl:call-template name="PropertySet"/> -->
        <xs:attribute ref="fmm:value" use="required"/>
      </xs:complexType>
    </xs:element>
  </xsl:template>    
  <!-- ========================================================================= -->
  <!-- RefFeature template processes the tag 'Feature'. -->
  <xsl:template name="RefFeature">
    <xs:element ref="fmm:{@fm:value}"/>
    <!--<xsl:comment>000 (<xsl:value-of select="node()"/>,<xsl:value-of select="@fm:value"/>)</xsl:comment>
    <xsl:choose>
      <xsl:when test="(fm:Cardinality/@fm:cardMax='+') or (fm:Cardinality/@fm:cardMax='*')">
        <xsl:comment>111</xsl:comment>
        <xs:element ref="fmm:{@fm:value}">
        <xsl:comment>222</xsl:comment>
          <xsl:attribute name="minOccurs">
            <xsl:value-of select="fm:Cardinality/@fm:cardMin"/>
          </xsl:attribute>
          <xsl:attribute name="maxOccurs">
            <xsl:value-of select="'unbounded'"/>
          </xsl:attribute>
        </xs:element>
        <xsl:comment>333</xsl:comment>
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="(fm:Cardinality/@fm:cardMax&gt;=1)">
          <xs:element ref="fmm:{@fm:value}">
            <xsl:attribute name="minOccurs">
              <xsl:value-of select="fm:Cardinality/@fm:cardMin"/>
            </xsl:attribute>
            <xsl:attribute name="maxOccurs">
              <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
            </xsl:attribute>
          </xs:element>
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>-->
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- RefFeatureMinZero template processes the tag 'Feature'. -->
  <xsl:template name="RefFeatureMinOccursZero">
    <xsl:if test="(fm:Cardinality/@fm:cardMax&gt;=1)">
      <xs:element ref="fmm:{@fm:value}">
        <xsl:attribute name="minOccurs">
          <xsl:value-of select="0"/>
        </xsl:attribute>
        <xsl:attribute name="maxOccurs">
          <xsl:value-of select="fm:Cardinality/@fm:cardMax"/>
        </xsl:attribute>
      </xs:element>
    </xsl:if>
    <xsl:if test="(fm:Cardinality/@fm:cardMax='+') or (fm:Cardinality/@fm:cardMax='*')">
      <xs:element ref="fmm:{@fm:value}">
        <xsl:attribute name="minOccurs">
          <xsl:value-of select="0"/>
        </xsl:attribute>
        <xsl:attribute name="maxOccurs">
          <xsl:value-of select="'unbounded'"/>
        </xsl:attribute>
      </xs:element>
    </xsl:if>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- RefCallMacro template processes the tag 'CallMacro'. -->
  <xsl:template name="RefCallMacro">
    <xsl:variable name="mName" select="@fm:value"/>
    <!-- XXX O.R. Marco_Name ma dve zvlastnosti: 1) ma podrzitko a 2) nema namespace -->
    <xsl:choose>
      <xsl:when test="//fm:RootFeature[@fm:value=$mName]">
        <xsl:for-each select="//fm:RootFeature[@fm:value=$mName]">
          <xsl:for-each select="fm:GroupedFeature">
            <xsl:comment> ####################################### </xsl:comment>
            <xsl:call-template name="RefFeature"/>
            <xsl:comment> ####################################### </xsl:comment>
          </xsl:for-each>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <xsl:message terminate="yes">ERROR: Called macro <xsl:value-of select="$mName"/> does NOT exist.</xsl:message>
      </xsl:otherwise>
    </xsl:choose>
    
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- RefCallMacroMinOccursZero template processes the tag 'CallMacro'. -->
  <xsl:template name="RefCallMacroMinOccursZero">
    <xsl:variable name="mName" select="fm:MacroNameProperty/@fm:value"/>
    <!-- XXX O.R. Marco_Name ma dve zvlastnosti: 1) ma podrzitko a 2) nema namespace -->
    <xsl:for-each select="//fm:MacroFeatureNode[@fm:value=$mName]">
      <xsl:for-each select="fm:GroupedFeature">
        <xsl:call-template name="RefFeatureMinOccursZero"/>
      </xsl:for-each>
    </xsl:for-each>
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template name="PropertySet">
    <!-- It is now called separately
        <xsl:if test="fm:Annotation">
            <xsl:call-template name="Annotation"/>
        </xsl:if> 
    -->
    <xsl:if test="fm:Attribute/fm:Integer/fm:IntegerProperties">
      <xsl:call-template name="IntegerProperties"/>
    </xsl:if>
    <xsl:if test="fm:Attribute/fm:String/fm:StringProperties">
      <xsl:call-template name="StringProperties"/>
    </xsl:if>
    <xsl:if test="fm:Attribute/fm:Float/fm:FloatProperties">
      <xsl:call-template name="FloatProperties"/>
    </xsl:if>
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template name="IntegerProperties">
    <!--<xsl:if test="fm:Attribute/fm:Integer/fm:IntegerProperties/fm:IntegerDefaultValue/@fm:value">-->
      <xs:element ref="fmm:{@fm:value}PropertySet"/>
    <!--</xsl:if>-->
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template name="StringProperties">
    <!--<xsl:if test="fm:Attribute/fm:String/fm:StringProperties/fm:StringDefaultValue/@fm:value">-->
      <xs:element ref="fmm:{@fm:value}PropertySet"/>
    <!--</xsl:if>-->
  </xsl:template>
  <!-- ========================================================================= -->
  <xsl:template name="FloatProperties">
    <!--<xsl:if test="fm:Attribute/fm:Float/fm:FloatProperties/fm:FloatDefaultValue/@fm:value">-->
      <xs:element ref="fmm:{@fm:value}PropertySet"/>
    <!--</xsl:if>-->
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- Description template extracts information from feature types and write them into 'annotation' element. -->
  <xsl:template name="Description">
    <!-- was Description -->
    <xs:annotation>
      <xs:documentation>
        <xsl:value-of select="fm:Annotation/fm:Description/@fm:value"/>
      </xs:documentation>
    </xs:annotation>
  </xsl:template>
  <!-- ========================================================================= -->
  <!-- The template creates a global element called 'Description' where the developer of an application can write any commnets.  -->
  <xsl:template name="CreateDescriptionElement">
    <xs:element name="Description">
      <xs:complexType mixed="true">
        <xs:sequence>
          <xs:any minOccurs="0" processContents="lax"/>
        </xs:sequence>
      </xs:complexType>
    </xs:element>
  </xsl:template>
</xsl:stylesheet>
