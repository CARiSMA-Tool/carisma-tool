package carisma.core.reports.beans;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AnalysisResultMessageBean {
	
	private String message;
	private StatusType status;
	public AnalysisResultMessageBean(AnalysisResultMessage msg) {
		this.message = msg.getText();
		this.status = msg.getStatus();
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public StatusType getStatus() {
		return status;
	}
	public void setStatus(StatusType status) {
		this.status = status;
	}
	
	

}
