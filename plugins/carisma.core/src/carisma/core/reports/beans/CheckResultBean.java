package carisma.core.reports.beans;

import java.io.Serializable;
import java.util.ArrayList;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.CheckResult;
import carisma.core.analysis.result.StatusType;

/**
 * JavaBean for serialization (e.g. JSON) of CheckResults.
 * 
 * @author Julian Flake <flake@uni-koblenz.de>
 * 
 */
public class CheckResultBean implements Serializable {

	private static final long serialVersionUID = -2586312446610802791L;

	private String name;
	private boolean success;
	private StatusType status;
	private ArrayList<AnalysisResultMessageBean> messages;

	public CheckResultBean() {
		super();
	}

	public CheckResultBean(CheckResult cr) {
		super();
		this.name = cr.getName();
		this.success = cr.isSuccessful();
		this.status = cr.getStatus();
		this.messages = new ArrayList<AnalysisResultMessageBean>();
		for (AnalysisResultMessage msg : cr.getResults()) {
			AnalysisResultMessageBean bean = new AnalysisResultMessageBean(msg);
			this.messages.add(bean);
		}
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isSuccess() {
		return success;
	}

	public void setSuccess(boolean successful) {
		this.success = successful;
	}

	public StatusType getStatus() {
		return status;
	}

	public void setStatus(StatusType status) {
		this.status = status;
	}
	
	public ArrayList<AnalysisResultMessageBean> getMessages() {
		return messages;
	}

	public void setMessages(ArrayList<AnalysisResultMessageBean> messages) {
		this.messages = messages;
	}

	@Override
	public String toString() {
		return "CheckResultBean [name=" + name + ", success=" + success + ", messages=" + messages + "]";
	}

}
