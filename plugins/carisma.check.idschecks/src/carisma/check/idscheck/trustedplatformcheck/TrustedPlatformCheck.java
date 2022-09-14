package carisma.check.idscheck.trustedplatformcheck;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;

public class TrustedPlatformCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.idscheck.trustedplatformcheck";
	public static final String CHECK_NAME = "UMLsec4ids Trusted Platform Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;

	
	public TrustedPlatformCheck() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public final boolean perform(final Map<String, CheckParameter> parameters, final AnalysisHost newHost) {
	    if (newHost != null) {
	        this.analysisHost = newHost;
	    } else {
	        this.analysisHost = new DummyHost(true);
	    }
		Resource currentModel = this.analysisHost.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			this.analysisHost.appendLineToReport("Empty model");
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package) {
			this.model = (Package) currentModel.getContents().get(0);
			return startCheck();
		}
		this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		this.analysisHost.appendLineToReport("Content is not a model!");
		return false;
	}	
	
	private boolean startCheck() {
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		boolean hasAllStereotypes = true;
		for (int i = 0; i < nodeList.size(); i++) {
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.VERIFIED) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes are not verified"));
				this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " is missing the <<verified>> Stereotype");
				hasAllStereotypes = false;
			}
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.ENCRYPTION) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes do not use encryption"));
				this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " is missing the <<encryption>> Stereotype");
				hasAllStereotypes = false;
			}
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.ISOLATED) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes are not isolated"));
				this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " is missing the <<isolated>> Stereotype");
				hasAllStereotypes = false;
			}
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.CERTIFIED) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes are not certified"));
				this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " is missing the <<certified>> Stereotype");
				hasAllStereotypes = false;
			}
		}
		return hasAllStereotypes;
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
