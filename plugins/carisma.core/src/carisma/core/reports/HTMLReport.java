package carisma.core.reports;

import org.apache.commons.text.StringEscapeUtils;

import carisma.core.analysis.result.AnalysisResult;

/**
 * An HTML report created from an AnalysisResult.
 */
public class HTMLReport {

	private final String html;

	public HTMLReport(AnalysisResult result) {
		// new...
		final var htmlOpen = """
				<!DOCTYPE html>
				<html lang="de">
				<head>
					<meta charset="utf-8">
					<meta name="viewport" content="width=device-width, initial-scale=1.0">
					<title>CARiSMA Report</title>
				</head>
				<body>
					<p>
					""";
		final var htmlClose = """
				</p>
				</body>
				</html>""";
		var htmlBody = StringEscapeUtils.escapeHtml4(result.getReport());
		htmlBody = htmlBody.replace("\t", "&emsp;").replaceAll("[\\r\\n]", "<br/>" + System.lineSeparator() + "\t");
		htmlBody = htmlBody.replaceAll("INFO:", "");
		htmlBody = htmlBody.replaceAll("ERROR:", "<span style=\"color:#ff0000;font-weight:bold;\">ERROR:</span>");
		htmlBody = htmlBody.replaceAll("WARNING:", "<span style=\"color:#FFBF00;font-weight:bold;\">WARNING:</span>");
		this.html = (htmlOpen + htmlBody + htmlClose);
	}

	public String getHtml() {
		return html;
	}

	@Override
	public String toString() {
		return getHtml();
	}

}
