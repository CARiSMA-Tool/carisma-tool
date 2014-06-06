<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform 
xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:p="http://maven.apache.org/POM/4.0.0"
xmlns="http://maven.apache.org/POM/4.0.0"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" 
version="1.0">
<xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes"/>
<xsl:template match="node()|@*" priority="-1">
  <xsl:copy>
    <xsl:apply-templates select="node()|@*"/>
  </xsl:copy>
</xsl:template>
<xsl:template match="/p:project/p:version">
<xsl:copy>XXXVERSIONXXX</xsl:copy>
</xsl:template>
</xsl:transform>
