package carisma.core.reports;

import org.json.JSONObject;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.reports.beans.AnalysisResultBean;

/**
 * 
 * A JSON report containing HTML report, XML report and analysis results for
 * individual representations.
 * 
 * @author Julian Flake <flake@uni-koblenz.de>
 */
public class JSONReport {

	private final static int INDENT = 2;
	private final JSONObject jsonObj;

	public JSONReport(final AnalysisResult result) {
		AnalysisResultBean bean = new AnalysisResultBean(result);
		this.jsonObj = new JSONObject(bean);
	}

	public String getJson() {
		return this.jsonObj.toString(INDENT);
	}

	@Override
	public String toString() {
		return this.jsonObj.toString(INDENT);
	}

}
