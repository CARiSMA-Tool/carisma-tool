package carisma.check.sequencediagramcrypto.report;

import java.util.ArrayList;
import java.util.List;

import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;

public class TimeDeltaReport implements Report {
	
	public static long	id	= 0l;
	
	public final long	startTime, reportTime;
	
	public TimeDeltaReport(long startTime, long reportTime) {
		this.startTime = startTime;
		this.reportTime = reportTime;
	}
	
	@Override
	public List<AnalysisResultMessage> getAnalysisResultMessages() {
		List<AnalysisResultMessage> list = new ArrayList<AnalysisResultMessage>();
		{
			String message = String.format("[ID: %08d][TIME: %08d]", TimeDeltaReport.id,
					(this.reportTime - this.startTime));
			list.add(new AnalysisResultMessage(StatusType.INFO, message));
		}
		return list;
	}
	
}
