package carisma.evolution;

import java.util.List;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


public class DeltaFactoryCheck implements CarismaCheck {
		
	private static final String CHANGE_REGISTER_NAME = "carisma.data.evolution.changes"; 
	
	private static final String DELTA_REGISTER_NAME = "carisma.data.evolution.deltas";

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
	    
		List<Change> changes = null;
		try {
			changes = (List<Change>) host.getFromRegister(CHANGE_REGISTER_NAME);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		if (changes == null) {
			return false;
		}
		if (changes.isEmpty()) {
			return false;
		}
		DeltaFactory theFactory = new DeltaFactory();
		List<Delta> computedDeltas = theFactory.getDeltas(changes);
		DeltaList deltas = new DeltaList(computedDeltas);
		if (host.isRegisterInUse(DELTA_REGISTER_NAME)) {
			try {
				host.removeFromRegister(DELTA_REGISTER_NAME);
			} catch (RegisterNotInUseException e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
		}
		try {
			host.putToRegister(DELTA_REGISTER_NAME, deltas);
		} catch (RegisterInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		host.addResultMessage(new AnalysisResultMessage(
		        StatusType.INFO, deltas.remainingSize() + " Deltas built (max. used changes: " + deltas.getHighestChangeCountAllTime() + ")."));
		return true;
	}
	
}
