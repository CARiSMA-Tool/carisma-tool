package carisma.check.extension4ids.usagecontrol;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;

import org.eclipse.uml2.uml.Model;

import org.eclipse.uml2.uml.Package;

import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheckWithID;

/** Analyzes a deployment diagram for usage control validation
 * @author Sanjeev Sun Shakya
 *
 */

public class UsageControlCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.extension4idschecks.usagecontrol";
	public static final String CHECK_NAME = "Extension4ids Usage Control Check";
	
	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
	    AnalysisHost host;
	    if (newHost != null) {
	        host = newHost;
	    } else {
	        host = new DummyHost(true);
	    }
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
		    host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Model)) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		boolean noErrors = true;
		Package model = (Package) currentModel.getContents().get(0);
		UsageControl check = new UsageControl(host);
		if (check.checkUsageControl(model) > 0) {
			for (AnalysisMessage errorMessage : check.getErrorMessages()) {
				if (errorMessage.getType() == StatusType.ERROR) {
					noErrors = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : check.getErrorMessages()) {
				errorMessage.print(host);
			}
			return noErrors;
		}
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