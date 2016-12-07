package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.check.sequencediagramcrypto.fol.FOLTerm;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class AttackerTermReport implements Report {
	
	// TODO : Better Formating?
	private final static String	MSG_TEXT	= "[MITMA][Term:%s][Evaluation:%b]";
	
	private final FOLTerm		term;
	private final boolean		eval;
	
	public AttackerTermReport(FOLTerm term, boolean eval) {
		this.term = term;
		this.eval = eval;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			StatusType t;
			if (this.eval) {
				t = StatusType.INFO;
			}
			else {
				t = StatusType.WARNING;
			}
			
			String message = String.format(AttackerTermReport.MSG_TEXT, this.term.toString(),
					this.eval);
			list.add(new AnalysisResultMessage(t, message));
		}
		return list;
	}
	
}
