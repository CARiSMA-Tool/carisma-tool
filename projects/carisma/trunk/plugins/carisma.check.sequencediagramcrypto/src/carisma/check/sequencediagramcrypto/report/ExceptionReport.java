package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class ExceptionReport implements Report {
	
	private final Exception	exception;
	
	public ExceptionReport(Exception exception) {
		this.exception = exception;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			list.add(new AnalysisResultMessage(StatusType.WARNING, this.exception
					.getLocalizedMessage()));
		}
		return list;
	}
	
}
