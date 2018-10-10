package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class StringReport implements Report {
	
	private final String		content;
	private final StatusType	statusType;
	
	public StringReport(String content) {
		this.content = content;
		this.statusType = StatusType.INFO;
	}
	
	public StringReport(String content, StatusType warning) {
		this.content = content;
		this.statusType = warning;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			list.add(new AnalysisResultMessage(this.statusType, this.content));
		}
		return list;
	}
	
}
