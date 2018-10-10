package carisma.check.sequencediagramcrypto.report;

import java.util.List;

import carisma.core.analysis.result.AnalysisResultMessage;

public interface Report {
	List<AnalysisResultMessage> getAnalysisResultMessages();
}
