<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:import href="carisma_results_to_html.xsl"/>
<xsl:template match="/">
  <Results>
    <Id>CARiSMA</Id>
    <Success>
      <xsl:value-of select="AnalysisResult/Status='SUCCESS'"/>
    </Success>
    <Tests>
      <Executed>
        <xsl:number value="count(AnalysisResult/CheckResult)"/>
      </Executed>
      <Failed>
        <xsl:number value="count(AnalysisResult/CheckResult[successful='false'])"/>
      </Failed>
      <Successful>
        <xsl:number value="count(AnalysisResult/CheckResult[successful='true'])"/>
      </Successful>
    </Tests>
    <Html>
      <xsl:text disable-output-escaping="yes">&lt;![CDATA[</xsl:text>
        <xsl:call-template name="carisma_html"/>
      <xsl:text disable-output-escaping="yes">]]&gt;</xsl:text>
    </Html>
  </Results>
</xsl:template>
</xsl:stylesheet>