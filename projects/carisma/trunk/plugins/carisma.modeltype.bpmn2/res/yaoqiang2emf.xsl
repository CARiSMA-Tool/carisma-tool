<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:output method="xml" encoding="UTF-8" standalone="yes" indent ="yes"/>

  <xsl:template match="*">
    <xsl:copy>
      <xsl:copy-of select="@*[name() != 'sourceElement' and name() != 'targetElement']"/>
      <xsl:apply-templates/>
    </xsl:copy>
  </xsl:template>

</xsl:transform>