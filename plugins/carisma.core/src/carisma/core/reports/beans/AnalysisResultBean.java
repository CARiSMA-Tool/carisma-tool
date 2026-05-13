package carisma.core.reports.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import carisma.core.analysis.result.AnalysisResult;
import carisma.core.analysis.result.AnalysisResultStatus;
import carisma.core.analysis.result.CheckResult;
import carisma.core.reports.HTMLReport;
import carisma.core.reports.XMLReport;

/**
 * JavaBean for serialization (e.g. JSON) of AnalysisResults.
 * 
 * @author Julian Flake <flake@uni-koblenz.de>
 * 
 */
public class AnalysisResultBean implements Serializable {
	private static final long serialVersionUID = 924813006585383438L;
	private String name;
	private Boolean success;
	private List<CheckResultBean> checks;
	private Map<String, String> reports;

	public AnalysisResultBean() {
		super();
	}

	public AnalysisResultBean(AnalysisResult result) {
		super();
		this.name = result.getName();
		this.success = result.getStatus().equals(AnalysisResultStatus.SUCCESS);
		this.checks = new ArrayList<CheckResultBean>();
		for (CheckResult cr : result.getCheckResults()) {
			this.checks.add(new CheckResultBean(cr));
		}
		this.reports = new HashMap<>();
		this.reports.put("HTML", new HTMLReport(result).getHtml());
		this.reports.put("XML", new XMLReport(result).getXml());
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public Boolean getSuccess() {
		return success;
	}

	public void setSuccess(Boolean success) {
		this.success = success;
	}

	public List<CheckResultBean> getChecks() {
		return checks;
	}

	public void setChecks(List<CheckResultBean> checks) {
		this.checks = checks;
	}

	public Map<String, String> getReports() {
		return reports;
	}

	public void setReports(Map<String, String> reports) {
		this.reports = reports;
	}

	@Override
	public String toString() {
		return "AnalysisResultBean [name=" + name + ", success=" + success + ", checks=" + checks + ", reports="
				+ reports + "]";
	}

}
