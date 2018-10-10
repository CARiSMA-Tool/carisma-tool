package carisma.core.checks;

import java.util.Map;

import carisma.core.analysis.AnalysisHost;

public interface CarismaCheck {

	/**
	 * Performs the execution of the check
	 * @param parameters
	 * @param host
	 * @return true, if the analysis result is "successful"
	 */
	boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host);
	
}
