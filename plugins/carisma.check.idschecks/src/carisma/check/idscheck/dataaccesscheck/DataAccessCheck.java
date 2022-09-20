package carisma.check.idscheck.dataaccesscheck;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EObjectResolvingEList;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityFinalNode;
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
		ArrayList<Action> actionList = (ArrayList<Action>) UMLHelper.getAllElementsOfType(model, Action.class);
		System.out.println("Actions ---------- " + actionList);
		ArrayList<ActivityPartition> partitionList = (ArrayList<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//System.out.println("Partitions ---------- " + partitionList);
		//List<ActivityPartition> partitionList2 = (List<ActivityPartition>) UMLHelper.getAllElementsOfType(model, ActivityPartition.class);
		//System.out.println("partitionList -------------- " + partitionList2);
		boolean ownerOrConsumer = true;
		boolean exactOneConsumerAndOwner = true;
		boolean existingPath = true;
		int numberOwner = 0;
		int numberConsumer = 0;
		List<Object> taggedValuesProtectedOwner = null;
		
		
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
				//partitionList.get(i).getValue(UMLsec.OWNER);
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

		ActivityDiagramManager adm = new ActivityDiagramManager(model, analysisHost);
		this.pathsList = adm.getAllPaths();
		if (pathsList.size() < 1) {
			existingPath = false;
		}
		//List<List<Element>> differentPaths;
		//list list string impl mit null
		ArrayList<ArrayList<String>> listOfDifferentPaths= new ArrayList<ArrayList<String>>();
		for (int i = 0; i < pathsList.size(); i++) {
			List<Element> currentPath = pathsList.get(i);
			//System.out.println("current path -------" + pathsList.get(i));
			//list string impl mit null
			ArrayList<String> listOfSinglePath = new ArrayList<>();
			for (int z = 0; z < currentPath.size(); z++) {
				String path = ((NamedElement) currentPath.get(z)).getName();
				//System.out.println("current action ---- " + path);
				//list string append path
				listOfSinglePath.add(path);
			}
			listOfDifferentPaths.add(listOfSinglePath);
			//list string append list list string
		}
		System.out.println("list of different paths ------ " + listOfDifferentPaths);
		
		
		//list erstellen mit allen verschiedenen protected actions
		ArrayList<String> protectedActions = new ArrayList<String>();
		List<Element> elementOwner = (List<Element>) UMLsecUtil.getStereotypedElements(model, UMLsec.OWNER);
		for(int x = 0; x < elementOwner.size(); x++) {
			taggedValuesProtectedOwner = UMLsecUtil.getTaggedValues("protected", UMLsec.OWNER, elementOwner.get(x));
			System.out.println("taggedvalues ---------------" + taggedValuesProtectedOwner);
			for (int y = 0; y < taggedValuesProtectedOwner.size(); y++) {
				String currentTag = ((NamedElement) taggedValuesProtectedOwner.get(y)).getName();
				protectedActions.add(currentTag);
			}

		}
		System.out.println("protectedactions ---------------" + protectedActions);
		// über pfade iterieren und schauen, ob ein element von protected actions drinnen liegt
		// wenn ja dann attribute und aktionen vergleichen und schauen ob req erfüllt ist
		
		for (int i = 0; i < listOfDifferentPaths.size(); i++) {
			ArrayList<String> checkPath = listOfDifferentPaths.get(i);
			for (int x = 0; x < checkPath.size(); x++) {
				
			}
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
