package carisma.evolution.uml2.umlchange;

import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.Change;


/** 
 * Wraps the UMLchangeParser in a check and puts the parser results in the registry.
 */

public class UMLchangeParserCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.evolution.uml2.umlchange.UMLchangeParserCheck";
	
	public static final String CHECK_NAME = "UMLchange Parser";
	
    /** Named used to register.
     */
	private static final String CHANGE_REGISTER_NAME = "carisma.data.evolution.changes"; 
	
	@Override
    public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost host) {
	    AnalysisHost theHost = host;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			return false;
		}
		Model theModel = (Model) currentModel.getContents().get(0);
		if (theModel == null) {
			return false;
		}
		UMLchangeParser theParser = new UMLchangeParser(theModel, theHost);
		List<Change> changes = theParser.generateDeltaDescriptions();
		try {
			if (host.isRegisterInUse(CHANGE_REGISTER_NAME)) {
				host.removeFromRegister(CHANGE_REGISTER_NAME);
			}
		} catch (RegisterNotInUseException e) {
		    Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		try {
			host.putToRegister(CHANGE_REGISTER_NAME, changes);
		} catch (RegisterInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		host.addResultMessage(new AnalysisResultMessage(
		        StatusType.INFO, "Parsed model for Changes with the UMLchange parser. Found " + changes.size() + " changes."));
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