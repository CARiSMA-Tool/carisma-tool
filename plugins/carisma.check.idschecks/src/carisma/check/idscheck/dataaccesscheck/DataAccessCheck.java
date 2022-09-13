package carisma.check.idscheck.dataaccesscheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;



import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;

public class DataAccessCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.idscheck.dataaccesscheck";
	public static final String CHECK_NAME = "UMLsec4ids Data Access Control Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;
    


	/**
	 * ArrayList with lists that represents the paths through the given ActivityDiagram.
	 */
	private List<List<Element>> pathsList = new ArrayList<>();
    
	public DataAccessCheck() {
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
		//Check if  User is Consumer and Owner
		//System.out.println(ActivityDiagramManager().getAllPaths());
		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		System.out.println("PathsLIST -------------- " + pathsList);
		ArrayList<Action> actionList = (ArrayList<Action>) UMLHelper.getAllElementsOfType(model, Action.class);
		//System.out.println("Actions ---------- " + actionList);
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//System.out.println("Partitions ---------- " + partitionList);
		boolean ownerOrConsumer = true;
		boolean exactOneConsumerAndOwner = true;
		int numberOwner = 0;
		int numberConsumer = 0;

		for (int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER) && UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "User is Owner and Consumer"));
				this.analysisHost.appendLineToReport(partitionList.get(i).getName() + " has Owner and Consumer Stereotype");
				ownerOrConsumer = false;
			}
			if (UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER)) {
				numberConsumer ++;
			}
			if (UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				numberOwner ++;
				partitionList.get(i).getValue(UMLsec.OWNER);
			}
		}
		if (numberOwner != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Owners for this interaction"));
			this.analysisHost.appendLineToReport("Only one Owner should be modeled in this Interaction");
			exactOneConsumerAndOwner = false;
		}
		if (numberConsumer != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Consumers for this interaction"));
			this.analysisHost.appendLineToReport("Only one Consumer should be modeled in this Interaction");
			exactOneConsumerAndOwner = false;
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
