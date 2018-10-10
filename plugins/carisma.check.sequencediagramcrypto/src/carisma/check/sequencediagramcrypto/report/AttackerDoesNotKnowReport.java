package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.knowledge.Knowledge;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AttackerDoesNotKnowReport implements Report {
	
	final private static String	MSG_TEXT	= "MITMA does not know %s";
	
	final private Knowledge		know;
	
	public AttackerDoesNotKnowReport(Knowledge know) {
		this.know = know;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			String message = String
					.format(AttackerDoesNotKnowReport.MSG_TEXT, this.know.toString());
			list.add(new AnalysisResultMessage(StatusType.INFO, message));
		}
		return list;
	}
	
}
