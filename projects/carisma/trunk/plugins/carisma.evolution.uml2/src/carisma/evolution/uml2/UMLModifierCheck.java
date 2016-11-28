package carisma.evolution.uml2;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class UMLModifierCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.evolution.uml2.UMLModifierCheck";
	
	public static final String POSTCONDITION_MODIFIERS_REGISTER_KEY = "carisma.data.evolution.modifiers"; 
	
	public static final String CHECK_NAME = "UML2 Model Modifier";
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel == null) {
			return false;
		}
		
		ModifierMap deltaModifiers = new ModifierMap(currentModel);
		if (host.isRegisterInUse(POSTCONDITION_MODIFIERS_REGISTER_KEY)) {
			try {
				host.removeFromRegister(POSTCONDITION_MODIFIERS_REGISTER_KEY);
			} catch (RegisterNotInUseException e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
		}
		try {
			host.putToRegister(POSTCONDITION_MODIFIERS_REGISTER_KEY, deltaModifiers);
		} catch (RegisterInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "UML Model Modifier initialized."));
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
