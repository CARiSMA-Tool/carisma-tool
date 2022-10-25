package carisma.check.idscheck.dataaccesscheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityFinalNode;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.NamedElement;



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
	
	public ArrayList<ActivityPartition> getAllSubpartitions (ActivityPartition mainPartition, ArrayList<ActivityPartition> allPartitions) {
		allPartitions.add(mainPartition);
		EList<ActivityPartition> allSubpartitions = mainPartition.getSubpartitions();
		for (int x = 0; x < allSubpartitions.size(); x++) {
			getAllSubpartitions (allSubpartitions.get(x), allPartitions);
		}
		return allPartitions;
	}
	
	private boolean startCheck() {
		//Check if  User is Consumer and Owner
		//System.out.println(ActivityDiagramManager().getAllPaths());
		/*
		List<List<Element>> pathsWithFinal = null;
		for (int x = 0; x < pathsList.size(); x++) {
			boolean hasFinalActivity = pathsList.get(x).contains(ActivityFinalNode.class);
			if (hasFinalActivity) {
				pathsWithFinal.add(pathsList.get(x));
			}

		}
		for (int z = 0; z < pathsWithFinal.size(); z++) {
			System.out.println("pathwithfinal --------- " + pathsWithFinal.get(z));
		}
		*/
		//Get all Partitions of the current model
		//TODO : get all subpartitions as well
		
		ArrayList<Action> actionList = (ArrayList<Action>) UMLHelper.getAllElementsOfType(model, Action.class);
		//System.out.println("Actions ---------- " + actionList);
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//System.out.println("Partitions ---------- " + partitionList);
		//List<ActivityPartition> partitionList2 = (List<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//System.out.println("partitionList -------------- " + partitionList2);
		//boolean ownerOrConsumer = true;
		//boolean exactOneConsumerAndOwner = true;
		//boolean existingPath = true;
		//boolean hasProtectedAction = true;
		int numberOwner = 0;
		int numberConsumer = 0;
		boolean checkSuccessful = true;
		
		
		/*
		List <Element> elementsOwner = UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		System.out.println("Element Owner ----------------" + elementsOwner);
		List <Element> elementsConsumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		System.out.println("Element Consumer ----------------" + elementsConsumer);

		for (int x = 0; x < elementsOwner.size(); x++) {
			List<String> stringValuesOwner = UMLsecUtil.getStringValues("requested_attributes_and_actions", UMLsec.OWNER, elementsOwner.get(x));
			System.out.println("stringValuesOwner------------ " + stringValuesOwner);
			
			List<Object> stringValuesProtectedOwner = UMLsecUtil.getTaggedValues("protected", UMLsec.OWNER, elementsOwner.get(x));
			System.out.println("stringValuesProtectedOwner------------ " + stringValuesProtectedOwner);
			
			List<String> stringValuesConsumer = UMLsecUtil.getStringValues("attributes_and_actions", UMLsec.CONSUMER, elementsConsumer.get(x));
			System.out.println("stringValuesConsumer--------- " + stringValuesConsumer);
		}
		*/
		//----------------------------------------------------------------------------------------------
		// iterate over all Partitions and get the number of Partitions with Consumer/Owner Stereotype
		
		for (int i = 0; i < partitionList.size(); i++) {
			System.out.println("current partition ---- " + partitionList.get(i));
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
				//partitionList.get(i).getValue(UMLsec.OWNER);
			}
		}
		//----------------------------------------------------------------------------------------------
		//Check if the number of Consumers and Owner is exactly one, if not fail the check and give relevant error messages
		
		if (numberOwner != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Owners for this interaction"));
			this.analysisHost.appendLineToReport("Only one Owner should be modeled in this Interaction");
			checkSuccessful = false;
		}
		if (numberConsumer != 1) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Too much Consumers for this interaction"));
			this.analysisHost.appendLineToReport("Only one Consumer should be modeled in this Interaction");
			checkSuccessful = false;
		}
		//----------------------------------------------------------------------------------------------
		//Get all Subpartitions of the Owner and Consumer Stereotype
		ArrayList<String> namesAllPartitionsOwner = new ArrayList<String>();
		ArrayList<ActivityPartition> allPartitionsOwner = new ArrayList<ActivityPartition>();
		for(int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.OWNER)) {
				getAllSubpartitions (partitionList.get(i), allPartitionsOwner);
				System.out.println("all subpartitions owner -- " + allPartitionsOwner);
				for(int x = 0; x < allPartitionsOwner.size(); x++) {
					namesAllPartitionsOwner.add(allPartitionsOwner.get(x).getName());
				}
			}
		}
		//System.out.println("names subpartitions owner -- " + namesAllPartitionsOwner);

		ArrayList<String> namesAllPartitionsConsumer = new ArrayList<String>();
		ArrayList<ActivityPartition> allPartitionsConsumer = new ArrayList<ActivityPartition>();
		for(int i = 0; i < partitionList.size(); i++) {
			if(UMLsecUtil.hasStereotype(partitionList.get(i), UMLsec.CONSUMER)) {
				getAllSubpartitions (partitionList.get(i), allPartitionsConsumer);
				//System.out.println("all subpartitions owner -- " + allPartitionsConsumer);
				for(int x = 0; x < allPartitionsConsumer.size(); x++) {
					namesAllPartitionsConsumer.add(allPartitionsConsumer.get(x).getName());
				}
			}
		}
		//System.out.println("names subpartitions consumer -- " + namesAllPartitionsConsumer);

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
		// Get all the names of the nodes within the different valid paths of the diagram
		// TODO : delete inital and final nodes from the list
		
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
		//System.out.println("elements different paths --------------- " + pathsList);

		//-------------------------------------------------------------------------------
		//----------------------------------------------------------------------------------------------		
		//list erstellen mit allen verschiedenen protected actions
		List<String> taggedValuesReqActOwner = null;
		List<String> taggedValuesReqAttOwner = null;
		List<Object> taggedValuesProtectedOwner = null;
		ArrayList<String> protectedActions = new ArrayList<String>();
		List<Element> elementOwner = (List<Element>) UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		for(int x = 0; x < elementOwner.size(); x++) {
			taggedValuesProtectedOwner = UMLsecUtil.getTaggedValues("protected", UMLsec.OWNER, elementOwner.get(x));
			System.out.println("taggedvalues ---------------" + taggedValuesProtectedOwner);
			for (int y = 0; y < taggedValuesProtectedOwner.size(); y++) {
				String currentTag = ((NamedElement) taggedValuesProtectedOwner.get(y)).getName();
				protectedActions.add(currentTag);
			}
			taggedValuesReqAttOwner = UMLsecUtil.getStringValues("requested_attributes", UMLsec.OWNER, elementOwner.get(x));
			taggedValuesReqActOwner = UMLsecUtil.getStringValues("requested_actions", UMLsec.OWNER, elementOwner.get(x));

			System.out.println("liste der req att and ac : " + taggedValuesReqAttOwner);
		}
		
		List<String> taggedValuesActConsumer = null;
		List<Element> elementConsumer = UMLsecUtil.getStereotypedElements(model, UMLsec.CONSUMER);
		List<String> taggedValuesAttConsumer = null;
		for(int x = 0; x < elementConsumer.size(); x++) {
			taggedValuesAttConsumer = UMLsecUtil.getStringValues("attributes", UMLsec.CONSUMER, elementConsumer.get(x));
			taggedValuesActConsumer = UMLsecUtil.getStringValues("actions", UMLsec.CONSUMER, elementConsumer.get(x));
		

		}
		System.out.println("list attributes consumer : " + taggedValuesAttConsumer);
		if(taggedValuesAttConsumer.equals(taggedValuesReqAttOwner)) {
			System.out.println("true");
		}
		//System.out.println("protectedactions ---------------" + protectedActions);
		//System.out.println("protectedactions ---------------" + taggedValuesProtectedOwner);

		//--------------------------------------------------------------------------------------------
		//Check if the protected actions is empty
		if(protectedActions.size() == 0) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Owner is missing a protected Action"));
			this.analysisHost.appendLineToReport("The Owner is missing a protected Action");
			checkSuccessful = false;
		}
		//----------------------------------------------------------------------------------------------
		//Check if the protected action of the Owner Stereotype is in the Owner Partition
		for(int i = 0; i < actionList.size(); i++) {
			for(int x = 0; x < taggedValuesProtectedOwner.size(); x++) {
				if(actionList.get(i) == taggedValuesProtectedOwner.get(x)) {
					for(int z = 0; z < actionList.get(i).getInPartitions().size(); z++) {
						if(namesAllPartitionsOwner.contains(actionList.get(i).getInPartitions().get(z).getName()) == false) {
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Owner protects an Action that someone else executes"));
							this.analysisHost.appendLineToReport("The Owner protects an Action that someone else executes : " + actionList.get(i).getName());
							checkSuccessful = false;
						}
					}
				}
			}
		}
		
		for(int i = 0; i < listOfDifferentPaths.size(); i++) {
			
		
		
			//all actions for owner and his subpartitions for current path
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
			//System.out.println("all actions from owner " + namesActionsOwner);
			//System.out.println("all actions from owner for current path " + namesActionsOwnerCurrentPath + listOfDifferentPaths.get(i));

			//all actions consumer and his subpartitions for current path
			
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
			//System.out.println("all actions from consumer " + namesActionsConsumer);
			//System.out.println("all actions from consumer for current path " + namesActionsConsumerCurrentPath + listOfDifferentPaths.get(i));
			
			for(int x = 0; x < listOfDifferentPaths.get(i).size(); x++) {
				//System.out.println("momentaner pfad " + listOfDifferentPaths.get(i));
				//System.out.println("protected actions " + protectedActions);
				//System.out.println("alle aktionen für mom pfad für owner "+namesActionsOwnerCurrentPath);
				System.out.println("momentanes element "+listOfDifferentPaths.get(i).get(x));
				System.out.println("momentaner index " +x );
				if((protectedActions.contains(listOfDifferentPaths.get(i).get(x))) && (namesActionsOwnerCurrentPath.contains(listOfDifferentPaths.get(i).get(x)))) {
					//System.out.println("-----hier----");
					List<String> pathBeforeProtectedAction = listOfDifferentPaths.get(i).subList(0, x);
					System.out.println("path before protected action " + pathBeforeProtectedAction) ;
					System.out.println("path before protected action size " + pathBeforeProtectedAction.size());
					System.out.println("names actions consumer path" + namesActionsConsumerCurrentPath);
					boolean fromConsumer = false;
					for(int z = 0; z < pathBeforeProtectedAction.size(); z++) {
						System.out.println("names actions consumer path" + namesActionsConsumerCurrentPath);
						System.out.println("path before prot " + pathBeforeProtectedAction.get(z));
						System.out.println("vgl : " +namesActionsConsumerCurrentPath.contains(pathBeforeProtectedAction.get(z)));
						if(namesActionsConsumerCurrentPath.contains(pathBeforeProtectedAction.get(z))) {
							fromConsumer = true;
						}
					}
					if(fromConsumer == true) {
						if((taggedValuesAttConsumer.containsAll(taggedValuesReqAttOwner) && taggedValuesReqAttOwner.containsAll(taggedValuesAttConsumer))== false) {
							System.out.println("attribute stimmen nicht überein");
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Attributes of the Owner and Consumer do not match"));
							this.analysisHost.appendLineToReport("The Attributes of the Owner and Consumer do not match for Owner Attributes : " + taggedValuesReqAttOwner + " and Consumer Attributes : " + taggedValuesAttConsumer);
							checkSuccessful = false;
						}
						if(taggedValuesReqActOwner.containsAll(taggedValuesActConsumer) == false) {
							System.out.println("mehr actions als zugelassen");
							this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Consumer wants to perform more Actions than allowed"));
							this.analysisHost.appendLineToReport("The Consumer wants to perform the Actions : " + taggedValuesActConsumer + " but only these are allowed : " + taggedValuesReqActOwner);
							checkSuccessful = false;
						}
						
					}
					if(fromConsumer == false) {
						this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "Access Request not from Consumer"));
						this.analysisHost.appendLineToReport("The Access Request does not come from a Consumer");
						checkSuccessful = false;
					}
					
				}
			}
			
		}
		//----------------------------------------------------------------------------------------------
		//Check if predecessor action of the protected action is in from an Owner
		/*
		boolean consumerRequest = true;
		for(int i = 0; i < taggedValuesProtectedOwner.size(); i++) {
			boolean comesFromConsumer = false;
			Object currentProtectedAction12 = taggedValuesProtectedOwner.get(i);
			for(int x = 0; x < pathsList.size(); x++) {
				if(pathsList.get(x).contains(taggedValuesProtectedOwner.get(i))) {
					List<Element> currentPath = pathsList.get(x);
					for(int z = 0; z < pathsList.get(x).size(); z++) {
						Element currentPathCurrentElement = pathsList.get(x).get(z);
						System.out.println("current path " + pathsList.get(x).get(z));
						for(int c = 0; c < actionList.size(); c++) {
							if(currentPathCurrentElement == actionList.get(c)) {
								for(int d = 0; d < actionList.get(c).getInPartitions().size(); d++) {
									if(UMLsecUtil.hasStereotype(actionList.get(c).getInPartitions().get(d), UMLsec.CONSUMER)) {
										comesFromConsumer = true;
									}
								}					
							}
						}
					}
				}
				// wird doppelt hinzugefügt testen eine klammer raus
				if(comesFromConsumer == false) {
					this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "The Access Request does not come from a Consumer"));
					this.analysisHost.appendLineToReport("For protected Action : " + taggedValuesProtectedOwner.get(i) + " the Access Request does not come from an Consumer");
					consumerRequest = false;
				}
			}
			System.out.println("comes from consumer " + comesFromConsumer);
		}
		*/
		for(int i = 0; i < taggedValuesProtectedOwner.size(); i++) {
			for(int x = 0; x < pathsList.size(); x++) {
				
			}
		}
		//----------------------------------------------------------------------------------------------
		
		//----------------------------------------------------------------------------------------------
		//Check if successor action of the protected action goes to an Owner again
		//----------------------------------------------------------------------------------------------

		
		// search for the protected action within the paths 
		// über pfade iterieren und schauen, ob ein element von protected actions drinnen liegt
		// wenn ja dann attribute und aktionen vergleichen und schauen ob req erfüllt ist
		
		for (int i = 0; i < listOfDifferentPaths.size(); i++) {
			ArrayList<String> checkPath = listOfDifferentPaths.get(i);
			for (int x = 0; x < checkPath.size(); x++) {
				
			}
		}
		//---------------------------------------------------------------------------------------
		//Check if any of the rules of Data Access Control rules are broken
		
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
