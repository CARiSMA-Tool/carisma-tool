package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AttackerKnowReport implements Report {
	
	final private static String	MSG_TEXT	= "MITMA knows %s";
	
	final private Knowledge		know;
	
	public AttackerKnowReport(Knowledge know) {
		this.know = know;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			String message = String.format(AttackerKnowReport.MSG_TEXT, this.know.toString());
			list.add(new AnalysisResultMessage(StatusType.WARNING, message));
		}
		return list;
	}
	
}
