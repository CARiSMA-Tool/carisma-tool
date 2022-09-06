package carisma.check.idscheck.identitymanagementcheck;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.UMLPackage;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;
import carisma.modeltype.uml2.UMLHelper;


public class IdentityManagementCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.idscheck.identitymanagementcheck";
	public static final String CHECK_NAME = "UMLsec4ids Identity Management Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;

	public IdentityManagementCheck() {
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
		ArrayList<Element> x509List = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.X509);
		ArrayList<Element> x509TLSList = (ArrayList<Element>) UMLsecUtil.getStereotypedElements(this.model, UMLsec.X509TLS);
		System.out.println("-----------x509-----------" + x509List.size());
		System.out.println("-----------x509TLS--------" + x509List.size());
		System.out.println("-----------Node-----------" + x509List.size());
		ListIterator<Element>
        iterator = x509List.listIterator();
		int elemX509 = 0;

		// Printing the iterated value
		System.out.println("\nUsing ListIterator:\n");
		while (iterator.hasNext()) {
			elemX509 ++;
			System.out.println("Value is : " + iterator.next());
		}
		System.out.println("------------elem:" + elemX509);
		for (int i = 0; i < nodeList.size();i++) 
	      { 		      
	          System.out.println("----------------------" + nodeList.get(i)); 		
	      } 
		if ((x509List.size() < nodeList.size()) && x509TLSList.size() == nodeList.size()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes miss the <<X509>> Stereotype"));
			this.analysisHost.appendLineToReport("Nodes miss the <<X509>> Stereotype");
			return false;
		}
		
		if ((x509List.size() == nodeList.size()) && x509TLSList.size() < nodeList.size()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Nodes miss the <<X509TLS>> Stereotype"));
			this.analysisHost.appendLineToReport("Nodes miss the <<X509TLS>> Stereotype");
			return false;
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
