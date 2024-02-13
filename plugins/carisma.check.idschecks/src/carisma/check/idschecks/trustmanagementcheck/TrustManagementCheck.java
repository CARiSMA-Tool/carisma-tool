package carisma.check.idschecks.trustmanagementcheck;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.NamedElement;
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

/**
 * analyzes a deployment diagram with respect to trust management rules.
 * @author Alexander Peikert
 *
 */

public class TrustManagementCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.idscheck.trustmanagementcheck";
	public static final String CHECK_NAME = "UMLsec4ids Trust Management Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;

	
	public TrustManagementCheck() {
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
	
	/**
	 * main function that starts the check.
	 * @return true if the model is correct according to trust management rules, false otherwise
	 */
	
	public boolean startCheck() {
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		ArrayList<CommunicationPath> commPathList = (ArrayList<CommunicationPath>) UMLHelper.getAllElementsOfType(model, CommunicationPath.class);
		
		boolean checkSuccessful = true;
		//Test if Nodes have one Security Profile
		for (int i = 0; i < nodeList.size(); i++) {	
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASEFREE) && (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASE) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS))) {
				if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASE)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<BaseFree>> and <<Base>>");
				}
				if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<BaseFree>> and <<Trust>>");
				}
				if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<BaseFree>> and <<Trustplus>>");
				}
				checkSuccessful = false;
			}
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASE) && (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS))) {
				if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<Base>> and <<Trust>>");
				}
				if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<Base>> and <<Trustplus>>");
				}
				checkSuccessful = false;
			}
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST) && UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes contain more than one Security Profile"));
					this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " contains both Stereotypes <<Trust>> and <<Trustplus>>");
					checkSuccessful = false;
			}
			if ((UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASEFREE) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASE) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUST) || UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.TRUSTPLUS)) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes have a missing Security Profile"));
				this.analysisHost.appendLineToReport(nodeList.get(i).getName() + " has no Security Profile");
				checkSuccessful = false;
			}
	}
	
	//------------------------------------------------------------------------------
	//Checks if there are Rules broken regarding Communication
	for (int i = 0; i < commPathList.size(); i++) {
		this.analysisHost.appendLineToReport("All Nodes that try to communicate must be named");
		EList<NamedElement> communicationMembers = commPathList.get(i).getMembers();
		String communicationMember1 = communicationMembers.get(0).getName();
		String communicationMember2 = communicationMembers.get(1).getName();
		for (int z = 0; z < nodeList.size(); z++) {
			String currentNode1 = nodeList.get(z).getName().toLowerCase();
			String currentNode2 = null;
			
			for (int x = 0; x < nodeList.size(); x++) {
				currentNode2 = nodeList.get(x).getName().toLowerCase();
				if ((communicationMember1.equals(currentNode1) || communicationMember1.equals(currentNode2)) && (communicationMember2.equals(currentNode1) || communicationMember2.equals(currentNode2))) {
					if (UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.BASEFREE) && UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUSTPLUS)) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Trust Plus Security Profile communicates with Base Free Security Profile"));
						this.analysisHost.appendLineToReport(nodeList.get(x).getName() + " has Trust+ Profile and tries to communicate with a Base Free Security Profile.");
					}
					if (UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.TRUSTPLUS) && UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.BASEFREE)) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Trust Plus Security Profile communicates with Base Free Security Profile"));
						this.analysisHost.appendLineToReport(nodeList.get(x).getName() + " has Trust+ Profile and tries to communicate with a Base Free Security Profile.");
					}
					if ((UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.TRUSTPLUS) || UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUST)) && UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.BASE)) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Trust Plus and Trust Security Profiles can reject communication with Base Security Profile"));
						this.analysisHost.appendLineToReport("Trust Plus and Trust Security Profiles can reject communication with Base Security Profile");
					}
					if (UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.BASE) && (UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUST) || UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUSTPLUS))) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Trust Plus and Trust Security Profiles can reject communication with Base Security Profile"));
						this.analysisHost.appendLineToReport("Trust Plus and Trust Security Profiles can reject communication with Base Security Profile");
					}
					if (UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.BASEFREE) && (UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUST) || UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.BASE))) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Basefree Security Profiles cannot communicate with Trust or Base Security Profile"));
						this.analysisHost.appendLineToReport("Basefree Security Profiles cannot communicate with Trust or Base Security Profile");
						checkSuccessful = false;
					}
					if ((UMLsecUtil.hasStereotype(nodeList.get(z), UMLsec.BASE) || UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.TRUST)) && UMLsecUtil.hasStereotype(nodeList.get(x), UMLsec.BASEFREE)) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Basefree Security Profiles cannot communicate with Trust or Base Security Profile"));
						this.analysisHost.appendLineToReport("Basefree Security Profiles cannot communicate with Trust or Base Security Profile");
						checkSuccessful = false;
					}
				}
			}
			
		}
	}
	//------------------------------------------------------------
	//Return if check is successful or not
	return checkSuccessful;
	
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
