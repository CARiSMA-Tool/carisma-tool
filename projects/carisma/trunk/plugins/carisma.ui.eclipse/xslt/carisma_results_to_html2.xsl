<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:template match="/"
              name="carisma_html">
  <html>
  <body>
  
   <xsl:variable name="alltests" select="count(AnalysisResult/CheckResult)" />
   <xsl:variable name="failedtests" select="count(AnalysisResult/CheckResult[successful!='true'])" />
              
   <table border="0" width="600px">
    <tr>
      <th>CARiSMA Security Analysis Results</th>
    </tr>
    <tr class="blank_row">
      <td><br/></td>
    </tr>
    <tr>
      <td>
        <center>The PA-system has been analyzed with CARiSMA and <xsl:value-of select="$alltests"/> checks have been executed.</center>
      </td>
    </tr>
    <tr class="blank_row">
      <td><br/></td>
    </tr>
    <tr>
      <td><center>
        <xsl:variable name="result" select="AnalysisResult/Status" />
        <xsl:if test="$result='SUCCESS'">
          No security violations have been detected.
        </xsl:if>
        <xsl:if test="$result!='SUCCESS'">
          Of the executed checks <xsl:value-of select="$failedtests"/> failed.        
        </xsl:if>
      </center></td>
    </tr>
    <tr class="blank_row">
      <td><br/></td>
    </tr>
    <tr>
      <td><center>The PA-system has the following security rating:</center></td>
    </tr>
    <tr>
      <td>
      <center>
      <table border="0">
        <tr>
          <td style="text-align: right;
            border: 0px solid #FFFFFF;
            padding: 0px;
            width:70px">Low Security</td>
          <td style="background-image:linear-gradient(90deg, red 0%, green 70%)">
            <table style="width:300px; 
              table-layout: fixed; 
              border: 0px">
                <tr>
                  <xsl:for-each select="AnalysisResult/CheckResult[successful='true']">
                    <td></td>
                  </xsl:for-each>
                  <td style="width:20px"><center>X</center></td>
                  <xsl:for-each select="AnalysisResult/CheckResult[successful!='true']">
                    <td></td>
                </xsl:for-each>
              </tr>
            </table>
          </td>
          <td style="text-align: left;
            border: 0px solid #FFFFFF;
            padding: 0px;
            width:70px">High Security</td>
        </tr>
      </table>
      </center>
      </td>
    </tr>
  </table>
  </body>
  </html>
</xsl:template>

</xsl:stylesheet>