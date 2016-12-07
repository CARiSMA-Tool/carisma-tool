package carisma.check.sequencediagramcrypto.report;

import java.util.List;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;

public class CheckReporter implements Reporter {
	
	final private AnalysisHost	host;
	
	public CheckReporter(AnalysisHost host) {
		this.host = host;
	}
	
	@Override
	public void report(Report report) {
		List<AnalysisResultMessage> messages = report.getAnalysisResultMessages();
		for (AnalysisResultMessage message : messages) {
			host.addResultMessage(message);
		}
	}
	
}
