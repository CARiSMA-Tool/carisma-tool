package carisma.check.idschecks.dataprovenancecheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.DecisionNode;
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
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.activity.ActivityDiagramManager;
import carisma.profile.umlsec.umlsec4ids.UMLsec;
import carisma.profile.umlsec.umlsec4ids.UMLsecUtil;

/**
 * analyzes an activity diagram with respect to data provenance tracking rules.
 * @author Alexander Peikert
 *
 */

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
	
	/**
	 * Return all subpartitions for a given main partition.
	 * @param mainPartition main partition for which all subpartitions should be returned
	 * @param allPartitions list in which the partition and subpartitions should be added
	 * @return a list that contains the main partition and all its subpartitions
	 */
	
	public ArrayList<ActivityPartition> getAllSubpartitions (ActivityPartition mainPartition, ArrayList<ActivityPartition> allPartitions) {
		allPartitions.add(mainPartition);
		EList<ActivityPartition> allSubpartitions = mainPartition.getSubpartitions();
		for (int x = 0; x < allSubpartitions.size(); x++) {
			getAllSubpartitions (allSubpartitions.get(x), allPartitions);
		}
		return allPartitions;
	}
	
	/**
	 * main function that starts the check.
	 * @return true if the model is correct according to data provenance tracking rules, false otherwise
	 */
	
	public boolean startCheck() {
		
		this.analysisHost.appendLineToReport("IMPORTANT");
		this.analysisHost.appendLineToReport("To run this check succcessfully you have to name every element!");
		this.analysisHost.appendLineToReport("Therefore give names not only to actions but to forks, joins, merges and decisions as well.");
		this.analysisHost.appendLineToReport("Only model within the given Activity.");
		this.analysisHost.appendLineToReport("--------------------------------------------------------------");

		ArrayList<ForkNode> forkList = (ArrayList<ForkNode>) UMLHelper.getAllElementsOfType(model, ForkNode.class);;
		ArrayList<DecisionNode> decisionList = (ArrayList<DecisionNode>) UMLHelper.getAllElementsOfType(model, DecisionNode.class);
		if(decisionList.size() > 0) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is a decision node the diagram"));
			this.analysisHost.appendLineToReport("There is a decision node within the diagram");
			this.analysisHost.appendLineToReport("If the stop action follows the start action in the same branch, then the check goes through.");
			this.analysisHost.appendLineToReport("If the start action is before the decision node and the stop action comes after the decision node, the check fails.");
			this.analysisHost.appendLineToReport("This is because the stop action could be avoided.");
			this.analysisHost.appendLineToReport("Remember to put the stop action in all branches in the diagram");
			this.analysisHost.appendLineToReport("----------------------------------------");
			this.analysisHost.appendLineToReport("If a prohibitted action is executed in any branch of the system, the Check fails as well.");
			this.analysisHost.appendLineToReport("There must not exist any possibility that an prohibitted action is executed.");
			this.analysisHost.appendLineToReport("----------------------------------------");
			this.analysisHost.appendLineToReport("If any branch of the system contains an action that is not permitted, the Check fail.");
			this.analysisHost.appendLineToReport("This is because there is a possibility that this action could be executed.");

		}
		if(forkList.size() > 0) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is a fork node the diagram"));
			this.analysisHost.appendLineToReport("There is a fork node within the diagram");
			this.analysisHost.appendLineToReport("The Check detects prohibitted actions that are executed.");
			this.analysisHost.appendLineToReport("It also detects actions that are executed but are not permitted.");
			this.analysisHost.appendLineToReport("If an obligation_start comes before the fork node and the obligation_stop comes after, the Check will not fail.");
			this.analysisHost.appendLineToReport("However there is the possibility that the check fails, if the obligation_start and obligation_stop both occur in the same parallelization.");

		}
		boolean checkSuccessful = true;
		//Check if there exists a path within the diagramm
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
			//get fork and decision nodes to create a warning 

			String activityName = activityList.get(i).getName();
			List<Object> taggedValuesClearingHouse = new ArrayList<Object>();
			List<Object> taggedValuesStartAction = new ArrayList<Object>();
			List<Object> taggedValuesStopAction = new ArrayList<Object>();
			List<Object> taggedValuesProtected = new ArrayList<Object>();
			//Get tagged values for the data provenance tracking activity
			if(UMLsecUtil.hasStereotype(activityList.get(i), UMLsec.DATAPROVENANCETRACKING)){
				taggedValuesClearingHouse = UMLsecUtil.getTaggedValues("clearing_house", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesStartAction = UMLsecUtil.getTaggedValues("start_action", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesStopAction = UMLsecUtil.getTaggedValues("stop_action", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				taggedValuesProtected = UMLsecUtil.getTaggedValues("protected", UMLsec.DATAPROVENANCETRACKING, activityList.get(i));
				//Check for amounts of actions in tags and an existing clearing house
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
				if((taggedValuesProtected.size() == 0) && (taggedValuesStopAction.size() > 0)) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is not an equal amount of Stop Actions and Protected Actions"));
					this.analysisHost.appendLineToReport("Stop Actions should be protected, but there are no Protected Actions in " + activityName);
					checkSuccessful = false;
				}
				//--------------------------------------------------			
				//Get names of tagged values
				ArrayList<String> namesTagStartAction = new ArrayList<String>();
				for(int x = 0; x < taggedValuesStartAction.size(); x++) {
					namesTagStartAction.add(((NamedElement) taggedValuesStartAction.get(x)).getName());
				}
				ArrayList<String> namesTagStopAction = new ArrayList<String>();
				for(int x = 0; x < taggedValuesStopAction.size(); x++) {
					namesTagStopAction.add(((NamedElement) taggedValuesStopAction.get(x)).getName());
				}
				
				
				ArrayList<String> namesTagProtected = new ArrayList<String>();
				for(int x = 0; x < taggedValuesProtected.size(); x++) {
					namesTagProtected.add(((NamedElement) taggedValuesProtected.get(x)).getName());
				}
				
				//Check if all stop actions are protected
				if(namesTagProtected.containsAll(namesTagStopAction) == false ){
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Stop Actions and Protected Actions are not equal"));
					this.analysisHost.appendLineToReport("Not all Stop Actions are protected " + activityName);
					checkSuccessful = false;
				}
				//-------------------------------------------------------------------------------------
				//Create start_stop_action pairs
				ArrayList<ArrayList<String>> startStopPairs = new ArrayList<ArrayList<String>>();
				if(namesTagStopAction.size() == namesTagStartAction.size()) {
					for(int w = 0; w < namesTagStartAction.size(); w++) {
						ArrayList<String> startStopPair = new ArrayList<String>();
						startStopPair.add(namesTagStartAction.get(w));
						startStopPair.add(namesTagStopAction.get(w));
						startStopPairs.add(startStopPair);
	
					}
				}
				//-----------------------------------------------------------------------------------------
				//Get all paths with names of actions in the diagram
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
				//Check if stop comes before start and check if stop comes after start
				boolean failStartStop = false;
				for(int x = 0; x < startStopPairs.size(); x++) {
					for(int z = 0; z < listOfDifferentPaths.size(); z++) {					
						if(listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(0)) && listOfDifferentPaths.get(z).contains(startStopPairs.get(x).get(1))) {
							int platzStart = -1;
							int platzStop = -1;
							for(int l = 0; l < listOfDifferentPaths.get(z).size(); l++) {
								if(listOfDifferentPaths.get(z).get(l).equals(startStopPairs.get(x).get(0))) {
									platzStart = l;
								}
								if(listOfDifferentPaths.get(z).get(l).equals(startStopPairs.get(x).get(1))) {
									platzStop = l;
								}
							}
							//Check if stop comes before start action
							if(platzStart > platzStop) {
								this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Obligation Start follows the Obligation Stop"));
								this.analysisHost.appendLineToReport(listOfDifferentPaths.get(z)  + " executes the Obligation Stop before the Obligation Start for Obligation : " + startStopPairs.get(x) + " in Activity : " + activityName);
								checkSuccessful = false;
								failStartStop = true;
							}
						}
						//check if start is execute, but stop not
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
				
				//get all actions from clearing house and its subpartitions
				ArrayList<String> namesActionsClearingHouse = new ArrayList<String>();
				for(int x = 0; x < actorSubPartitions.size(); x++) {
					EList<ActivityNode> nodes = actorSubPartitions.get(x).getNodes();
					for(int z = 0; z < nodes.size(); z++) {
						namesActionsClearingHouse.add(nodes.get(z).getName());
					}
				}
				//get all actions that are stop actions and check for execution in clearing house
				if(failStartStop == false) {
					for(int z = 0; z < namesTagProtected.size(); z++) {
						if(namesActionsClearingHouse.contains(namesTagProtected.get(z)) == false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Protected Actions are not executed by a Clearing House."));
							this.analysisHost.appendLineToReport("There is a protected Action " + namesTagProtected.get(z) + " that is not executed by a Clearing House in " + activityName);					
							checkSuccessful = false;
						}
	
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
