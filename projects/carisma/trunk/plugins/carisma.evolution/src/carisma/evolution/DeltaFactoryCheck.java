package carisma.evolution;

import java.util.List;
import java.util.Map;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


public class DeltaFactoryCheck implements CarismaCheckWithID {
		
	public static final String CHECK_ID = "carisma.evolution.DeltaFactoryCheck";

	public static final String PRECONDITION_CHANGE_REGISTER_NAME = "carisma.data.evolution.changes"; 
	
	public static final String PRECONDITION_DELTA_REGISTER_NAME = "carisma.data.evolution.deltas";
	
	public static final String CHECK_NAME = "Delta Calculator";

	@SuppressWarnings("unchecked")
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
	    
		List<Change> changes = null;
		try {
			changes = (List<Change>) host.getFromRegister(PRECONDITION_CHANGE_REGISTER_NAME);
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
		List<Delta> computedDeltas = DeltaFactory.getDeltas(changes);
		DeltaList deltas = new DeltaList(computedDeltas);
		if (host.isRegisterInUse(PRECONDITION_DELTA_REGISTER_NAME)) {
			try {
				host.removeFromRegister(PRECONDITION_DELTA_REGISTER_NAME);
			} catch (RegisterNotInUseException e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
		}
		try {
			host.putToRegister(PRECONDITION_DELTA_REGISTER_NAME, deltas);
		} catch (RegisterInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		host.addResultMessage(new AnalysisResultMessage(
		        StatusType.INFO, deltas.remainingSize() + " Deltas built (max. used changes: " + deltas.getHighestChangeCountAllTime() + ")."));
		return true;
	}

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}
}