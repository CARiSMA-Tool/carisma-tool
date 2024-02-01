package carisma.check.idscheck.datausagecheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
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
 * analyzes an activity diagram with respect to data usage control rules.
 * @author Alexander Peikert
 *
 */

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
	 * @return true if the model is correct according to data usage control rules, false otherwise
	 */
	
	public boolean startCheck() {
		this.analysisHost.appendLineToReport("----IMPORTANT-------");
		this.analysisHost.appendLineToReport("To execute this Check successfully, the Initial Node and Final Node must have an empty name");
		this.analysisHost.appendLineToReport("Decision Nodes, Fork Nodes, Merge Nodes and Join Nodes must have an empty name");
		this.analysisHost.appendLineToReport("This is because they cannot be added to the permission tag");


		boolean checkSuccessful = true;
		// check if there are existing valid paths within the diagram
		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		if (pathsList.size() < 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is no existing path through the diagram"));
			this.analysisHost.appendLineToReport("There is no existing path through the diagram");
			checkSuccessful = false;
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
		//-------------------------------------------------------------------------------
		//get fork and decision nodes to create a warning
		ArrayList<ForkNode> forkList = (ArrayList<ForkNode>) UMLHelper.getAllElementsOfType(model, ForkNode.class);;
		ArrayList<DecisionNode> decisionList = (ArrayList<DecisionNode>) UMLHelper.getAllElementsOfType(model, DecisionNode.class);
		if(decisionList.size() > 0) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is a decision node the diagram"));
			this.analysisHost.appendLineToReport("There is a decision node within the diagram");
			this.analysisHost.appendLineToReport("If the obligation_stop follows the obligation_start in the same branch, then the check goes through.");
			this.analysisHost.appendLineToReport("If the obligation_start is before the decision node and the obligation_stop comes after the decision node, the check fails.");
			this.analysisHost.appendLineToReport("This is because the stop could be avoided.");
			this.analysisHost.appendLineToReport("Remember to put the obligation_stop in all branches in the diagram");
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
		//------------------------------------------------------------------------
		ArrayList<String> namesProhibs = new ArrayList<String>();
		ArrayList<String> namesObligStart = new ArrayList<String>();
		ArrayList<String> namesObligStop = new ArrayList<String>();
		ArrayList<String> namesPerm = new ArrayList<String>();
		List<Object> taggedValuesProhibitions = null;
		List<Object> taggedValuesObligationStart = null;
		List<Object> taggedValuesObligationStop = null;
		List<Object> taggedValuesPermissions = null;
		ArrayList<String> validNodesForPath = new ArrayList<String>();
		//Get all Partitions within the diagram
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//Iterate over every Partition, check for data usage control stereotype
		for (int z = 0; z < partitionList.size(); z++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(z), UMLsec.DATAUSAGECONTROL)) {
				namesProhibs.clear();
				namesObligStart.clear();
				namesObligStop.clear();
				namesPerm.clear();
				validNodesForPath.clear();
				taggedValuesProhibitions = UMLsecUtil.getTaggedValues("prohibition", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
				taggedValuesObligationStart = UMLsecUtil.getTaggedValues("obligation_start", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
				taggedValuesObligationStop = UMLsecUtil.getTaggedValues("obligation_stop", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
				taggedValuesPermissions = UMLsecUtil.getTaggedValues("permission", UMLsec.DATAUSAGECONTROL, partitionList.get(z));
				
				//Check for subpartitions and nodes of subpartitions
			
				ArrayList<ActivityPartition> allPartitions = new ArrayList<ActivityPartition>();
				allPartitions = getAllSubpartitions(partitionList.get(z) , allPartitions);
				ArrayList<String> allPartitionNames = new ArrayList<String>();
				for(int i = 0; i < allPartitions.size(); i++) {
					String currentPartitionName = allPartitions.get(i).getName();
					allPartitionNames.add(currentPartitionName);
				}
				
				//Get names of all Nodes within a single AcitityPartition
				ArrayList<EList<ActivityNode>> anodesOfSinglePartition = new ArrayList<EList<ActivityNode>>() ;
				for(int i = 0; i < allPartitions.size(); i++) {
					anodesOfSinglePartition.add(allPartitions.get(i).getNodes());
				}			
				//----------------------------------------
				//Get names of all actions in (sub-)partitions
				ArrayList<String> allNodeNamesSubpartitions = new ArrayList<String>();
				for(int i = 0; i < anodesOfSinglePartition.size(); i++) {
					for(int c = 0; c < anodesOfSinglePartition.get(i).size(); c++) {
						allNodeNamesSubpartitions.add(anodesOfSinglePartition.get(i).get(c).getName());
					}
				}
				
				EList<ActivityNode> nodesOfSinglePartition = partitionList.get(z).getNodes();
				ArrayList<String> nameNodesSinglePartition = new ArrayList<String>();
				for (int c = 0; c < nodesOfSinglePartition.size(); c++) {
					nameNodesSinglePartition.add(nodesOfSinglePartition.get(c).getName());
				}
				
				//-------------------------------------------------------------------------------
				//For each valid path check which part of the path is in the current ActivityPartition + remove start and final node				
				for(int t = 0; t < listOfDifferentPaths.size(); t++) {
					for(int h = 0; h < allNodeNamesSubpartitions.size(); h++) {
						if(listOfDifferentPaths.get(t).contains(allNodeNamesSubpartitions.get(h)) && allNodeNamesSubpartitions.get(h) != null) {
							validNodesForPath.add(allNodeNamesSubpartitions.get(h));
						}
					}
				}

				//---------------------------------------------------------------------------------
				//Get a list of lists of paths, with the names of the actions executed, delete empty actions and remove duplicated paths
				ArrayList<ArrayList<String>> testList = new ArrayList<ArrayList<String>>();
				for(int h = 0; h < listOfDifferentPaths.size(); h++) {
					ArrayList<String> testValidNodes = new ArrayList<String>();
					for(int d = 0; d < listOfDifferentPaths.get(h).size(); d++) {
						for(int c = 0; c < allNodeNamesSubpartitions.size(); c++) {
							if(allNodeNamesSubpartitions.get(c) != null && listOfDifferentPaths.get(h).get(d) != null){
								if(listOfDifferentPaths.get(h).get(d).equals(allNodeNamesSubpartitions.get(c).toString())) {
									testValidNodes.add(allNodeNamesSubpartitions.get(c));

								}
							}
						}		
					}
					testList.add(testValidNodes);
				}
				int x = 0;
				int counter = testList.size();
				for(int h = 0; h < counter; h++) {
					if(testList.get(x).isEmpty()) {
						testList.remove(x);
						x--;
					}
					x++;
				}
				
				// testList remove duplicates 
				for(int i = 0; i < testList.size(); i++) {
					int g = i + 1;
					while(g < testList.size()) {
						if(testList.get(i).containsAll(testList.get(g)) && testList.get(g).containsAll(testList.get(i))) {
							testList.remove(testList.get(g));
							g --;
						}
					g++;
					}
				}

				//---------------------------------------------------------------------------
				//check if prohibition is executed
				for(int q = 0; q < taggedValuesProhibitions.size(); q++) {
					String currentProhib = ((NamedElement) taggedValuesProhibitions.get(q)).getName();
					namesProhibs.add(currentProhib);
				}
				
				for(int u = 0; u < validNodesForPath.size(); u++) {
					if(namesProhibs.contains(validNodesForPath.get(u))){
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Actor tries to execute a Prohibition!"));
						this.analysisHost.appendLineToReport(partitionList.get(z).getName()  + " tries to execute the prohibited action : " + validNodesForPath.get(u));
						checkSuccessful = false;

					}
				}
				
				//---------------------------------------------------------------
				//check for a matching number of obligation starts and stops
				for(int o = 0; o < taggedValuesObligationStart.size(); o++) {
					String currentObligStart = ((NamedElement) taggedValuesObligationStart.get(o)).getName();
					namesObligStart.add(currentObligStart);
				}
				for(int p = 0; p < taggedValuesObligationStop.size(); p++) {
					String currentObligStop = ((NamedElement) taggedValuesObligationStop.get(p)).getName();
					namesObligStop.add(currentObligStop);
				}
				if(namesObligStart.size() != namesObligStop.size()) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is not an equal amount of Obligation Starts and Obligation Stops"));
					this.analysisHost.appendLineToReport(partitionList.get(z).getName() + " has not an equal amount of Obligation Starts and Obligation Stops");
					checkSuccessful = false;
				}
				//Match the obligation start action to the obligation stop action
				ArrayList<ArrayList<String>> startStopPair = new ArrayList<ArrayList<String>>();
				if(namesObligStart.size() == namesObligStop.size()) {
					for(int w = 0; w < namesObligStart.size(); w++) {
						ArrayList<String> startStop = new ArrayList<String>();
						startStop.add(namesObligStart.get(w));
						startStop.add(namesObligStop.get(w));
						startStopPair.add(startStop);

					}
				}
				//--------------------------------------------------------------------------
				//check if obligation start and stop are executed
				for(int q = 0; q < startStopPair.size(); q++) {
					for(int g = 0; g < testList.size(); g++) {
						if(testList.get(g).contains(startStopPair.get(q).get(0)) && testList.get(g).contains(startStopPair.get(q).get(1))) {
							int platzStart = -1;
							int platzStop = -1;
							for(int l = 0; l < testList.get(g).size(); l++) {
								if(testList.get(g).get(l).equals(startStopPair.get(q).get(0))) {
									platzStart = l;
								}
								if(testList.get(g).get(l).equals(startStopPair.get(q).get(1))) {
									platzStop = l;
								}
							}
							
							//check if stop comes before start
							if(platzStart > platzStop) {
								this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Obligation Stop follows the Obligation Start"));
								this.analysisHost.appendLineToReport(testList.get(g)  + " executes the Obligation Stop before the Obligation Start for Obligation : " + startStopPair.get(q) + " in Partition : " + partitionList.get(z).getName());
								checkSuccessful = false;
							}
							
						}
						//Check if obligation star is executed but stop not
						if(testList.get(g).contains(startStopPair.get(q).get(0)) && testList.get(g).contains(startStopPair.get(q).get(1)) == false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Obligation Stop does not follow after the Obligation Start"));
							this.analysisHost.appendLineToReport(testList.get(g)  + " executes the Obligation Start but does not executes the Obligation Stop for Obligation : " + startStopPair.get(q) + " in Partition : " + partitionList.get(z).getName());
							checkSuccessful = false;
						}
						
					}
				}
				
				//-----------------------------------------------------------------------------------
				//check if all executed action are permitted
				for(int r = 0; r < taggedValuesPermissions.size(); r++) {
					String currentPerm = ((NamedElement) taggedValuesPermissions.get(r)).getName();
					namesPerm.add(currentPerm);
				}
				if(namesPerm.containsAll(validNodesForPath) == false) {
					for(int u = 0; u < validNodesForPath.size(); u++) {
						if(namesPerm.contains(validNodesForPath.get(u)) == false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Actor tries to execute an Action that is not permitted!"));
							this.analysisHost.appendLineToReport(partitionList.get(z).getName() + " tries to execute an Action that is not permitted! " + validNodesForPath.get(u));
							checkSuccessful = false;
						}
					}
				}
				//--------------------------------------------------------------------------
			}
			
			
		}
		
		//------------------------------
		//Return if Check is successful or not
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
