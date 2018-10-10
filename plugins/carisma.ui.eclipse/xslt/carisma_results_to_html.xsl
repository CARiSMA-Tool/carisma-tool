<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html"/>
<xsl:template match="/"
              name="carisma_html">
 <xsl:variable name="alltests" select="count(AnalysisResult/CheckResult)" />
   <xsl:variable name="failedtests" select="count(AnalysisResult/CheckResult[successful!='true'])" />
              
  <p>
    The PA-system has been analyzed with CARiSMA and <xsl:value-of select="$alltests"/> 
    <xsl:if test="$alltests=1">
    	privacy check has been executed.
    </xsl:if>
    <xsl:if test="$alltests>1">
    	privacy checks have been executed.
    </xsl:if>
  </p>
  <p>
    <xsl:variable name="result" select="AnalysisResult/Status" />
      <xsl:if test="$result='SUCCESS'">
        No privacy violations have been detected.
      </xsl:if>
      <xsl:if test="$result!='SUCCESS'">
        Of the executed privacy checks <xsl:value-of select="$failedtests"/> failed.        
      </xsl:if>
    </p>
    <p>
      The PA-system achieved the following privacy rating:
    </p>
    <p>
      <table border="0">
        <tr>
          <td border="0px" padding="0px" align="right">
            Low<br/>Privacy
          </td>
          <td background="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAS4AAAAoCAIAAADPDIC9AAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAADsMAAA7DAcdvqGQAAAMUSURBVHhe7dP5T88BHMfxp1zpcCRHJSWiJHKVoxyRKyUSIhJChUqESK5hzjlmjjlmmGuYOWaOOeaaY8bMMXPMMdccM2aO8f386C/w/uG1Pf6FB3/gdzl+ufHTjR/l+e5SgW8V+FqRL5UcnyvzyZ2P7nyownsP3nnw1pM3nrz24pUXL7154c3zqjyrxtPqPKnO4xo8cvHhoQ8PanLfl3u+3K3luFOb23W45VKXm37c8OO6P9f8uRrAlQAu1+NSIBcDuVCf80GcC+JsMGeCOd2AUyGcDOFEQ8fxRhwL5WgoRxpzuInjUBgHwzgQzv5w9jVlbwR7ItjdjF2R7IxkR3O2u7RgWxRbo9jSks0t2dSKja3Z0Jr1bVjX1rE2mjXRrI5hVQwr27GiHcvbs6wDSzuwpCOLY1kUy8I4FsQxvxPzOjO3M2VdHKVdmRXPzHhKujG9u6M4gakJFPVwFPakoBf5vZjUmwl9yOtDbiI5iYzvy9gkspMYk8zoZLL6MTKFzBRG9CdjgGNoKumpDBnI4IGkpZE2iNRBDBhMyhBHcjpJ6fQdSuIwertk0DODHsNJGE63EcRn0jWTLiPplEVcFrGj6DiK9qMdMWOIzqZtNm3G0mqcI2o8LXJonkNkLhG5NM0jPI+wCTSeSOhEGk2iYT4h+QQXEFRA/UICCwmYjH8RfkXUnULtqQ7fYmoW4zONGtOp5jID7xl4leBZgsdM3GdRuZRKpVScTfnZuJVRrgzmyL9UURVV0QRVVEVVNEEVVVEVTVBFVVRFE1RRFVXRBFVURVU0QRVVURVNUEVVVEUTVFEVVdEEVVRFVTRBFVVRFU1QRVVURRNUURVV0QRVVEVVNEEVVVEVTVBFVVRFE1RRFVXRBFVURVU0QRVVURVNUEVVVEUTVFEVVdEEVVRFVTRBFVVRFU1QRVVURRNUURVV0QRVVEVVNEEVVVEVTVBFVVRFE1RRFVXRBFVURVU0QRVVURVNUEVVVEUTVFEVVdEEVVRFVTRBFVVRFU1QRVVURRNUURVV0QRVVEVVNEEVVVEV/785/AVG6ssC7Gw52wAAAABJRU5ErkJggg==">
            <table width="300px" border="0px">
                <tr>
                  <xsl:for-each select="AnalysisResult/CheckResult[successful='true']">
                    <td></td>
                  </xsl:for-each>
                  <td width="20px"><center>X</center></td>
                  <xsl:for-each select="AnalysisResult/CheckResult[successful!='true']">
                    <td></td>
                </xsl:for-each>
              </tr>
            </table>
          </td>
          <td border="0px" padding="0px" align="left">
            High<br/>Privacy
          </td>
        </tr>
      </table>
    </p>
</xsl:template>

</xsl:stylesheet>