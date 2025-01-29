package carisma.check.idscheck.transferprocessprotocol;

import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;

import carisma.check.staticcheck.securelinks.SecureLinks;
import carisma.check.staticcheck.securelinks.utils.AnalysisMessage;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

/**
 * @author Sanjeev Sun Shakya
 */
public class TransferProcessProtocolCheck implements CarismaCheckWithID {
	
	public static final String CHECK_ID = "carisma.check.extension4idschecks.datatransfer";
	public static final String CHECK_NAME = "Extension4ids Transfer Process Protocol Check";

	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
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
		Package model = (Package) currentModel.getContents().get(0);
		TransferProcessProtocol check = new TransferProcessProtocol(host);
		return check.checkDataTransferProtocol(model);
	}

	@Override
	public String getCheckID() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getName() {
		// TODO Auto-generated method stub
		return null;
	}

}
