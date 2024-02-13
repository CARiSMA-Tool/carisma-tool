package carisma.check.idscheck.dataaccesscheck;

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

/**
 * analyzes an activity diagram with respect to data access rules.
 * @author Alexander Peikert
 *
 */

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
	 * @return true if the model is correct according to data access control rules, false otherwise
	 */
	
	public boolean startCheck() {

		//Get all Partitions of the current model
		
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		
		int numberOwner = 0;
		int numberConsumer = 0;
		boolean checkSuccessful = true;
		
		
		//----------------------------------------------------------------------------------------------
		// iterate over all Partitions and get the number of Partitions with Consumer/Owner Stereotype
		
		for (int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER) && UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "User is Owner and Consumer"));
				this.analysisHost.appendLineToReport(partitionList.get(i).getName() + " has Owner and Consumer Stereotype");
				checkSuccessful = false;
			}
			if (UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER)) {
				numberConsumer ++;
			}
			if (UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				numberOwner ++;
			}
		}
		//----------------------------------------------------------------------------------------------
		//Check if the number of Consumers and Owner is exactly one, if not fail the check and give relevant error messages
		
		if (numberOwner != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Owners for this interaction"));
			this.analysisHost.appendLineToReport("Exactly one Owner should be modeled in this Interaction");
			checkSuccessful = false;
		}
		if (numberConsumer != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Consumers for this interaction"));
			this.analysisHost.appendLineToReport("Exactly one Consumer should be modeled in this Interaction");
			checkSuccessful = false;
		}
		//----------------------------------------------------------------------------------------------
		//Get all Subpartitions of the Owner and Consumer Stereotype
		ArrayList<String> namesAllPartitionsOwner = new ArrayList<String>();
		ArrayList<ActivityPartition> allPartitionsOwner = new ArrayList<ActivityPartition>();
		for(int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				getAllSubpartitions (partitionList.get(i), allPartitionsOwner);
				for(int x = 0; x < allPartitionsOwner.size(); x++) {
					namesAllPartitionsOwner.add(allPartitionsOwner.get(x).getName());
				}
			}
		}
		ArrayList<String> namesAllPartitionsConsumer = new ArrayList<String>();
		ArrayList<ActivityPartition> allPartitionsConsumer = new ArrayList<ActivityPartition>();
		for(int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER)) {
				getAllSubpartitions (partitionList.get(i), allPartitionsConsumer);
				for(int x = 0; x < allPartitionsConsumer.size(); x++) {
					namesAllPartitionsConsumer.add(allPartitionsConsumer.get(x).getName());
				}
			}
		}

		//----------------------------------------------------------------------------------------------
				
		//----------------------------------------------------------------------------------------------
		//Check if there is at least one existing and valid path
		
		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		if (pathsList.size() < 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "There is no existing path through the diagram"));
			this.analysisHost.appendLineToReport("There is no existing path through the diagram");
			checkSuccessful = false;
		}
		//----------------------------------------------------------------------------------------------
		
		//get all valid paths within the diagram
		ArrayList<ArrayList<String>> listOfDifferentPaths= new ArrayList<ArrayList<String>>();
		for (int i = 0; i < pathsList.size(); i++) {
			ArrayList<String> listOfSinglePath = new ArrayList<>();
			for (int z = 0; z < pathsList.get(i).size(); z++) {
				String path = ((NamedElement) pathsList.get(i).get(z)).getName();
				listOfSinglePath.add(path);
			}
			listOfDifferentPaths.add(listOfSinglePath);
		}
		
		//-------------------------------------------------------------------------------
		//----------------------------------------------------------------------------------------------		
		//create lists with tagged values of owner
		List<String> taggedValuesReqActOwner = null;
		List<String> taggedValuesReqAttOwner = null;
		List<Object> taggedValuesProtectedOwner = null;
		ArrayList<String> protectedActions = new ArrayList<String>();
		List<Element> elementOwner = (List<Element>) UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		for(int x = 0; x < elementOwner.size(); x++) {
			taggedValuesProtectedOwner = UMLsecUtil.getTaggedValues("protected", UMLsec.OWNER, elementOwner.get(x));
			for (int y = 0; y < taggedValuesProtectedOwner.size(); y++) {
				String currentTag = ((NamedElement) taggedValuesProtectedOwner.get(y)).getName();
				protectedActions.add(currentTag);
			}
			taggedValuesReqAttOwner = UMLsecUtil.getStringValues("requested_attributes", UMLsec.OWNER, elementOwner.get(x));
			taggedValuesReqActOwner = UMLsecUtil.getStringValues("requested_actions", UMLsec.OWNER, elementOwner.get(x));

		}
		//create lists with tagged values of consumer
		List<String> taggedValuesActConsumer = null;
		List<Element> elementConsumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		List<String> taggedValuesAttConsumer = null;
		for(int x = 0; x < elementConsumer.size(); x++) {
			taggedValuesAttConsumer = UMLsecUtil.getStringValues("attributes", UMLsec.CONSUMER, elementConsumer.get(x));
			taggedValuesActConsumer = UMLsecUtil.getStringValues("actions", UMLsec.CONSUMER, elementConsumer.get(x));
		

		}
		
		
		//--------------------------------------------------------------------------------------------
		//Check if the protected actions is empty
		if(protectedActions.size() == 0) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Owner is missing a protected Action"));
			this.analysisHost.appendLineToReport("The Owner is missing a protected Action");
			checkSuccessful = false;
		}
		//----------------------------------------------------------------------------------------------
		//For each path, check if rules are broken
		for(int i = 0; i < listOfDifferentPaths.size(); i++) {
				
			//all actions for owner partition and his subpartitions for current path
			ArrayList<String> namesActionsOwnerCurrentPath = new ArrayList<String>();
			ArrayList<String> namesActionsOwner = new ArrayList<String>();
			for(int x = 0; x < allPartitionsOwner.size(); x++) {
				EList<ActivityNode> nodes = allPartitionsOwner.get(x).getNodes();
				for(int z = 0; z < nodes.size(); z++) {
					namesActionsOwner.add(nodes.get(z).getName());
				}
				for(int z = 0; z < listOfDifferentPaths.get(i).size(); z++) {
					if(namesActionsOwner.contains(listOfDifferentPaths.get(i).get(z))){
						namesActionsOwnerCurrentPath.add(listOfDifferentPaths.get(i).get(z));
					}
						
				}
			}
			
			//all actions consumer partition and his subpartitions for current path
			
			ArrayList<String> namesActionsConsumerCurrentPath = new ArrayList<String>();
			ArrayList<String> namesActionsConsumer = new ArrayList<String>();
			for(int x = 0; x < allPartitionsConsumer.size(); x++) {
				EList<ActivityNode> nodes = allPartitionsConsumer.get(x).getNodes();
				for(int z = 0; z < nodes.size(); z++) {
					namesActionsConsumer.add(nodes.get(z).getName());
				}
				for(int z = 0; z < listOfDifferentPaths.get(i).size(); z++) {
					if(namesActionsConsumer.contains(listOfDifferentPaths.get(i).get(z))){
						namesActionsConsumerCurrentPath.add(listOfDifferentPaths.get(i).get(z));
					}
						
				}
			}
			
			
			for(int x = 0; x < listOfDifferentPaths.get(i).size(); x++) {
				
				//check if current path executes protected action and if this action is executed by owner
				if((protectedActions.contains(listOfDifferentPaths.get(i).get(x))) && (namesActionsOwnerCurrentPath.contains(listOfDifferentPaths.get(i).get(x)))) {
					//get all action executed before the protected action
					List<String> pathBeforeProtectedAction = listOfDifferentPaths.get(i).subList(0, x);
					boolean fromConsumer = false;
					//check if access request comes from the consumer
					for(int z = 0; z < pathBeforeProtectedAction.size(); z++) {
						if(namesActionsConsumerCurrentPath.contains(pathBeforeProtectedAction.get(z))) {
							fromConsumer = true;
						}
					}
					//if request comes from consumer, check if attributes match and actions are allowed
					if(fromConsumer == true) {
						if((taggedValuesAttConsumer.containsAll(taggedValuesReqAttOwner) && taggedValuesReqAttOwner.containsAll(taggedValuesAttConsumer))== false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Attributes of the Owner and Consumer do not match"));
							this.analysisHost.appendLineToReport("The Attributes of the Consumer are not sufficient for the access request for Owner Attributes : " + taggedValuesReqAttOwner + " and Consumer Attributes : " + taggedValuesAttConsumer);
							checkSuccessful = false;
						}
						if(taggedValuesReqActOwner.containsAll(taggedValuesActConsumer) == false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Consumer wants to perform more Actions than allowed"));
							this.analysisHost.appendLineToReport("The Consumer wants to perform the Actions : " + taggedValuesActConsumer + " but only these are allowed : " + taggedValuesReqActOwner);
							checkSuccessful = false;
						}
						
					}
					//check if request comes from consumer
					if(fromConsumer == false) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Access Request not from Consumer"));
						this.analysisHost.appendLineToReport("The Access Request does not come from a Consumer");
						checkSuccessful = false;
					}
					
				}
			}
			
		}
		//----------------------------------------------------------------------------------------------
		//Return if any of the rules of Data Access Control rules are broken
		
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
