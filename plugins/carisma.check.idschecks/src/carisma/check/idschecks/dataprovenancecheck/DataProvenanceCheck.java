package carisma.check.idschecks.dataprovenancecheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.ForkNode;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.DummyHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;

public class DataProvenanceCheck implements CarismaCheckWithID {

	public static final String CHECK_ID = "carisma.check.idscheck.dataprovenancecheck";
	public static final String CHECK_NAME = "UMLsec4ids Data Provenance Check";

	/**
	 * the model to check.
	 */
	private Package model = null;
	
	/**
	 * AnalysisHost for report.
	 */
    private AnalysisHost analysisHost;
	private List<List<Element>> pathsList = new ArrayList<>();
	private List<List<ActivityPartition>> allPartitionsStopElem;


	
	public DataProvenanceCheck() {
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
	
	
	public ArrayList<ActivityPartition> getAllSubpartitions (ActivityPartition mainPartition, ArrayList<ActivityPartition> allPartitions) {
		allPartitions.add(mainPartition);
		EList<ActivityPartition> allSubpartitions = mainPartition.getSubpartitions();
		for (int x = 0; x < allSubpartitions.size(); x++) {
			getAllSubpartitions (allSubpartitions.get(x), allPartitions);
		}
		return allPartitions;
	}
	
	/*
	 * Plan
	 * alle pfade bekommen
	 * schauen ob start aktion im pfad
	 * schauen ob stop aktion im pfad
	 * schauen ob stop aktion in protected
	 * schauen ob clearing house right mit protected aktion hat
	 */
	private boolean startCheck() {

		boolean checkSuccessful = true;
		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		if (pathsList.size() < 1) {
			checkSuccessful = false;
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is no existing path through the diagram"));
			this.analysisHost.appendLineToReport("There is no existing path through the diagram");
		}
		//get activity with stereotype
		ArrayList<Activity> activityList = (ArrayList<Activity>) UMLHelper.getAllElementsOfType(model, Activity.class);
		for(int i = 0; i < activityList.size(); i++) {
			//System.out.println("current activity : " + activityList.get(i));
			String activityName = activityList.get(i).getName();
			List<Object> taggedValuesClearingHouse = new ArrayList<Object>();
			List<Object> taggedValuesStartAction = new ArrayList<Object>();
			List<Object> taggedValuesStopAction = new ArrayList<Object>();
			List<Object> taggedValuesProtected = new ArrayList<Object>();
			//System.out.println("has stereo : " + UMLsecUtil.hasStereotype(activityList.get(i), UMLsec.DATAPROVENANCETRACKING));
			if(UMLsecUtil.hasStereotype(activityList.get(i), UMLsec.DATAPROVENANCETRACKING)){
				System.out.println("hier");
				taggedValuesClearingHouse = UMLsecUtil.getTaggedValues("clearing_house", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesStartAction = UMLsecUtil.getTaggedValues("start_action", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesStopAction = UMLsecUtil.getTaggedValues("stop_action", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesProtected = UMLsecUtil.getTaggedValues("protected", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
			}
			// test for equal amounts of actions in tags
			if(taggedValuesClearingHouse.size() < 1) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is no existing Clearing House"));
				this.analysisHost.appendLineToReport("There is no existing Clearing House in " + activityName);
				checkSuccessful = false;
			}
			if(taggedValuesStartAction.size() != taggedValuesStopAction.size()) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is not an equal amount of Start Actions and Stop Actions"));
				this.analysisHost.appendLineToReport("There is not an equal amount of Start Actions and Stop Actions in " + activityName);
				checkSuccessful = false;
			}
			if(taggedValuesProtected.size() != taggedValuesStopAction.size()) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is not an equal amount of Stop Actions and Protected Actions"));
				this.analysisHost.appendLineToReport("There is not an equal amount of Stop Actions and Protected Actions in " + activityName);
				checkSuccessful = false;
			}
			//--------------------------------------------------			
			//check for equal names in protected and stop acitions
			ArrayList<String> namesTagStartAction = new ArrayList<String>();
			for(int x = 0; x < taggedValuesStartAction.size(); x++) {
				namesTagStartAction.add(((NamedElement) taggedValuesStartAction.get(x)).getName());
			}
			//System.out.println("names start " +namesTagStartAction);
			ArrayList<String> namesTagStopAction = new ArrayList<String>();
			for(int x = 0; x < taggedValuesStopAction.size(); x++) {
				namesTagStopAction.add(((NamedElement) taggedValuesStopAction.get(x)).getName());
			}
			//System.out.println("names stop " +namesTagStopAction);
			
			
			ArrayList<String> namesTagProtected = new ArrayList<String>();
			for(int x = 0; x < taggedValuesProtected.size(); x++) {
				namesTagProtected.add(((NamedElement) taggedValuesProtected.get(x)).getName());
			}
			//System.out.println("names protected " +namesTagProtected);
			
			if(namesTagProtected.containsAll(namesTagStopAction) == false || namesTagStopAction.containsAll(namesTagProtected) == false) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Stop Actions and Protected Actions are not equal"));
				this.analysisHost.appendLineToReport("The Stop Actions and Protected Actions are not equal in " + activityName);
				checkSuccessful = false;
			}
			//-------------------------------------------------------------------------------------
			//create start_stop_action pairs
			ArrayList<ArrayList<String>> startStopPairs = new ArrayList<ArrayList<String>>();
			if(namesTagStopAction.size() == namesTagStartAction.size()) {
				for(int w = 0; w < namesTagStartAction.size(); w++) {
					ArrayList<String> startStopPair = new ArrayList<String>();
					startStopPair.add(namesTagStartAction.get(w));
					startStopPair.add(namesTagStopAction.get(w));
					startStopPairs.add(startStopPair);

				}
			}
			//System.out.println("start stop pairs " + startStopPairs);
			//-----------------------------------------------------------------------------------------
			//über alle pfade testen ob wenn start erreicht wird auch stop erreicht wird
			//alle pfade bekommen
			ArrayList<ArrayList<String>> listOfDifferentPaths= new ArrayList<ArrayList<String>>();
			for(int x = 0; x < pathsList.size(); x++) {
				List<Element> currentPath = pathsList.get(x);
				ArrayList<String> listOfSinglePath = new ArrayList<>();
				for (int z = 0; z < currentPath.size(); z++) {
					String path = ((NamedElement) currentPath.get(z)).getName();
					listOfSinglePath.add(path);
				}
				listOfDifferentPaths.add(listOfSinglePath);
			}
			//System.out.println("different paths --------------- " + listOfDifferentPaths);
			//check if stop comes before start and check if stop comes after start
			boolean failStartStop = false;
			for(int x = 0; x < startStopPairs.size(); x++) {
				for(int z = 0; z < listOfDifferentPaths.size(); z++) {
					if(listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(0)) && listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(1))) {
						int platzStart = -1;
						int platzStop = -1;
						for(int l = 0; l < listOfDifferentPaths.get(z).size(); l++) {
							if(listOfDifferentPaths.get(z).get(l) == startStopPairs.get(z).get(0)) {
								platzStart = l;
							}
							if(listOfDifferentPaths.get(z).get(l) == startStopPairs.get(z).get(1)) {
								platzStop = l;
							}
						}
						if(platzStart > platzStop) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Obligation Start follows the Obligation Stop"));
							this.analysisHost.appendLineToReport(listOfDifferentPaths.get(z)  + " executes the Obligation Stop before the Obligation Start for Obligation : " + startStopPairs.get(x) + " in Activity : " + activityName);
							checkSuccessful = false;
							failStartStop = true;
							//System.out.println("In dem Pfad " + testList.get(g)+ " wird Stop vor Start ausgeführt");
						}
					}
					if(listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(0)) && listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(1)) == false) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Obligation Stop does not follow after the Obligation Start"));
						this.analysisHost.appendLineToReport(listOfDifferentPaths.get(z)  + " executes the Obligation Start but does not executes the Obligation Stop for Obligation : " + startStopPairs.get(x) + " in Activity : " + activityName);
						checkSuccessful = false;
						failStartStop = true;
					}
				}
			}
			//check if all stop actions/ protected actions are executed by a clearing house
			//get all partitions and subartitions for actors in clearing house tag
			ArrayList<ActivityPartition> actorSubPartitions = new ArrayList<ActivityPartition>() ;
			ArrayList<ActivityPartition> objectsToPartitions = new ArrayList<ActivityPartition>();
			for(int x = 0; x < taggedValuesClearingHouse.size(); x++) {
				objectsToPartitions.add((ActivityPartition) taggedValuesClearingHouse.get(x));
			}
			for(int x = 0; x < objectsToPartitions.size(); x++) {
				actorSubPartitions = getAllSubpartitions(objectsToPartitions.get(x) , actorSubPartitions);
			}
			//System.out.println("all actors with all subpartitions " + actorSubPartitions);
			
			//get all actions from clearing house and its subpartitions
			ArrayList<String> namesActionsClearingHouse = new ArrayList<String>();
			for(int x = 0; x < actorSubPartitions.size(); x++) {
				EList<ActivityNode> nodes = actorSubPartitions.get(x).getNodes();
				for(int z = 0; z < nodes.size(); z++) {
					namesActionsClearingHouse.add(nodes.get(z).getName());
				}
			}
			//System.out.println("names of all actions of clearing house : " + namesActionsClearingHouse);
			//get all actions that are stop actions
			//ArrayList<Action> stopActions = new ArrayList<Action>();			
			if(failStartStop == false) {
				//System.out.println("alle stop aktionen mit partitionen : " + taggedValuesStopAction);
				System.out.println("namen der stop aktionen : " + namesTagStopAction);
				System.out.println("namen der clearing aktionen : " + namesActionsClearingHouse);
				for(int z = 0; z < namesTagStopAction.size(); z++) {
					if(namesActionsClearingHouse.contains(namesTagStopAction.get(z)) == false) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Protected Actions are not executed by a Clearing House."));
						this.analysisHost.appendLineToReport("There is a protected Action " + namesTagStopAction.get(z) + " that is not executed by a Clearing House in " + activityName);					
						checkSuccessful = false;
					}

				}
				
			}

			//-------------------------------------------------------------------------------------
		
		}
		//-----------------------------------------------------------------------------------------
		
		
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
