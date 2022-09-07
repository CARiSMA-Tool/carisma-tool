package carisma.check.idschecks.trustmanagementcheck;

import java.util.ArrayList;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
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
	
	private boolean startCheck() {
		ArrayList<Node> nodeList = (ArrayList<Node>) UMLHelper.getAllElementsOfType(model, Node.class);
		ArrayList<Element> basefreeList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.BASEFREE);
		ArrayList<Element> baseList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.BASE);
		ArrayList<Element> trustList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.TRUST);
		ArrayList<Element> trustplusList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.TRUSTPLUS);
		boolean onlyOneProfile = true;
		for (int i = 0; i < nodeList.size(); i++) {
			int profile = 0 ;
			boolean hasBaseFree = false;
			boolean hasBase = false;
			boolean hasTrust = false;
			boolean hasTrustPlus = false;
			System.out.println("-------Iteriertes Objekt---------" + nodeList.get(i));
			
			if (UMLsecUtil.hasStereotype(nodeList.get(i), UMLsec.BASEFREE) == true) {
				hasBaseFree = true;
			}
			/*
			if (hasStereotype(nodeList[i], UMLsec.BASE) == true) {
				hasBase = true;
			}
			
			if (hasStereotype(nodeList[i], UMLsec.TRUST) == true) {
				hasTrust = true;
			}
			
			if (hasStereotype(nodeList[i], UMLsec.TRUSTPLUS) == true) {
				hasTrustPlus = true;
			}
		}
			 */
			System.out.println("--------------Iteriertes Objekt hat BASEFREE Stereotype-----------" + hasBaseFree);
		}
	return true;
	
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
