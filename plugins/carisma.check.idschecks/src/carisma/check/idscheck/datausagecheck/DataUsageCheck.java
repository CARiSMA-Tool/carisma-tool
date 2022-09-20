package carisma.check.idscheck.datausagecheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

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

public class DataUsageCheck implements CarismaCheckWithID {


	public static final String CHECK_ID = "carisma.check.idscheck.datausagecheck";
	public static final String CHECK_NAME = "UMLsec4ids Data Usage Control Check";

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
	
    
	public DataUsageCheck() {
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
		// check if there are existing valid paths within the diagram
		boolean existingPath = true;
		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		if (pathsList.size() < 1) {
			existingPath = false;
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is no existing path through the diagram"));
			this.analysisHost.appendLineToReport("There is no existing path through the diagram");
		}
		//--------------------------------------------------------------------------------
		
		//get all valid paths within the diagram
		ArrayList<ArrayList<String>> listOfDifferentPaths= new ArrayList<ArrayList<String>>();
		for (int i = 0; i < pathsList.size(); i++) {
			List<Element> currentPath = pathsList.get(i);
			ArrayList<String> listOfSinglePath = new ArrayList<>();
			for (int z = 0; z < currentPath.size(); z++) {
				String path = ((NamedElement) currentPath.get(z)).getName();
				listOfSinglePath.add(path);
			}
			listOfDifferentPaths.add(listOfSinglePath);
		}
		System.out.println("different paths --------------- " + listOfDifferentPaths);
		//-------------------------------------------------------------------------------
		
		ArrayList<String> namesProhibs = new ArrayList<String>();
		ArrayList<String> namesObligStart = new ArrayList<String>();
		ArrayList<String> namesObligStop = new ArrayList<String>();
		ArrayList<String> namesPerm = new ArrayList<String>();
		List<Object> taggedValuesProhibitions = null;
		List<Object> taggedValuesObligationStart = null;
		List<Object> taggedValuesObligationStop = null;
		List<Object> taggedValuesPermissions = null;
		ArrayList<String> validNodesForPath = new ArrayList<String>();
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		for (int z = 0; z < partitionList.size(); z++) {
			namesProhibs.clear();
			namesObligStart.clear();
			namesObligStop.clear();
			namesPerm.clear();

			taggedValuesProhibitions = UMLsecUtil.getTaggedValues("prohibition", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
			taggedValuesObligationStart = UMLsecUtil.getTaggedValues("obligation_start", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
			taggedValuesObligationStop = UMLsecUtil.getTaggedValues("obligation_stop", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
			taggedValuesPermissions = UMLsecUtil.getTaggedValues("permission", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
			//get names of all Nodes within a single AcitityPartition
			EList<ActivityNode> nodesOfSinglePartition = partitionList.get(z).getNodes();
			//System.out.println("nodes of single partition --- " + nodesOfSinglePartition);
			ArrayList<String> nameNodesSinglePartition = new ArrayList<String>();
			for (int c = 0; c < nodesOfSinglePartition.size(); c++) {
				nameNodesSinglePartition.add(nodesOfSinglePartition.get(c).getName());
			}
			System.out.println("Nodes: " + nameNodesSinglePartition);
			//-------------------------------------------------------------------------------
			//for each valid path check which part of the path is in the current ActivityPartition + remove start and final node
			for(int t = 0; t < listOfDifferentPaths.size(); t++) {
				for(int h = 0; h < nameNodesSinglePartition.size(); h++) {
					if(listOfDifferentPaths.get(t).contains(nameNodesSinglePartition.get(h)) && nameNodesSinglePartition.get(h) != null) {
						validNodesForPath.add(nameNodesSinglePartition.get(h));
					}
				}
				System.out.println("valid nodes ---- " + validNodesForPath);
			}
			//---------------------------------------------------------------------------------
			//check if prohibition is executed
			for(int q = 0; q < taggedValuesProhibitions.size(); q++) {
				String currentProhib = ((NamedElement) taggedValuesProhibitions.get(q)).getName();
				namesProhibs.add(currentProhib);
			}
			for(int g = 0; g < validNodesForPath.size(); g++) {
				if(namesProhibs.contains(validNodesForPath.get(g))){
					System.out.println("prohibition wird ausgeführt");
				}
			}
			//---------------------------------------------------------------
			for(int o = 0; o < taggedValuesObligationStart.size(); o++) {
				String currentObligStart = ((NamedElement) taggedValuesObligationStart.get(o)).getName();
				namesObligStart.add(currentObligStart);
			}
			for(int p = 0; p < taggedValuesObligationStop.size(); p++) {
				String currentObligStop = ((NamedElement) taggedValuesObligationStop.get(p)).getName();
				namesObligStop.add(currentObligStop);
			}
			
			//check if all executed action are permitted
			for(int r = 0; r < taggedValuesPermissions.size(); r++) {
				String currentPerm = ((NamedElement) taggedValuesPermissions.get(r)).getName();
				namesPerm.add(currentPerm);
			}
			if(namesPerm.containsAll(validNodesForPath) == false) {
				System.out.println("permission hat nicht alle aktionen aus nem legit pfad");
			}
			//--------------------------------------------------------------------------
			
			/*
			System.out.println("prohib: " + namesProhibs);
			System.out.println("oblig start: " + namesObligStart);
			System.out.println("oblig stop: " + namesObligStop);
			System.out.println("perm: " + namesPerm);	
			*/
			/*
			if(namesObligStart.size() != namesObligStop.size()) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is not an equal amount of Obligation Starts and Obligation Stops"));
				this.analysisHost.appendLineToReport(partitionList.get(z).getName() + "has not an equal amount of Obligation Starts and Obligation Stops");
			}
			if(nameNodesSinglePartition.containsAll(namesObligStop) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Obligation Stops not reachable"));
				this.analysisHost.appendLineToReport(partitionList.get(z).getName() + "has Obligation Stops that are not reachable");
			}
			*/
			
		}
		
		//------------------------------
		/*
		ArrayList<String> permissionActions = new ArrayList<String>();
		List<Element> elementDUC = (List<Element>) UMLsecUtil.getStereotypedElements(model, UMLsec.DATAUSAGECONTROL);
		for(int x = 0; x < elementDUC.size(); x++) {
			Object nodesOfPartition = ((ActivityPartition) elementDUC.get(x)).getNodes();
			//System.out.println("nodesofpartition" + nodesOfPartition);
			taggedValuesPermissions = UMLsecUtil.getTaggedValues("prohibition", UMLsec.DATAUSAGECONTROL, elementDUC.get(x));
			//System.out.println("tagged values -------------- " + taggedValuesPermissions);

			for (int y = 0; y < taggedValuesPermissions.size(); y++) {											
				String currentTag = ((NamedElement) taggedValuesPermissions.get(y)).getName();
				permissionActions.add(currentTag);
			}
			//System.out.println("prohibitions -------------- " + permissionActions);
		}
		*/
		
		
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
