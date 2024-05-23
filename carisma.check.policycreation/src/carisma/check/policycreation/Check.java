package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.ProfileApplication;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Type;
import org.json.JSONObject;

import carisma.modeltype.uml2.UMLHelper;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CarismaCheckWithID;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";

	public static final String CHECK_NAME = "Policy Model Transformation";
	
//--------------
	AnalysisHost host;
	Map<String,ExtendedJSONObject> odrlObjects = new HashMap<String, ExtendedJSONObject>();//TODO: if counter is used as ID, String may be replaced with int or map may be replaced with an array
	Map<String,Collection<ExtendedJSONObject>> typeBuckets = new HashMap<String,Collection<ExtendedJSONObject>>();
	ExtendedJSONObject root;
	//Map<JSONObject>

	//TODO remove
	int numOfElements = 0;
	//
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		System.out.println("Starting Policycheck performance");
		this.host = host;
		this.numOfElements = 0;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		for (EObject content : currentModel.getContents()) {
			System.out.println("Model content:" + content);
		}

		if (currentModel.getContents().get(0) instanceof Package) {
			
			Package model = (Package) currentModel.getContents().get(0);
			
			//
			for (Element e : model.allOwnedElements()) {
				System.out.println("Owned Element: " + e);
				if (e instanceof Action) {
					Action ea = (Action) e;
				}
				for (Stereotype st : e.getAppliedStereotypes()) {
					System.out.println("Stereotype: "+st);
					//st.get
				}
			}
			//
			/*
			System.out.print("Contained ProfileApplications: ");
			for (ProfileApplication t : model.getProfileApplications()) {
				System.out.print(t.toString());
			}
			TreeIterator<EObject> iterator = model.eAllContents();
			System.out.println();
			System.out.println("Complicated Iterator:");
			while (iterator.hasNext()) {
				EObject eobj = iterator.next();
				System.out.println(eobj);
				List<EObject> objlistpre = eobj.eContents();
				System.out.println(objlistpre.isEmpty());
			}
			List<EObject> objlist = model.eContents();
			System.out.println("Simple Iterator:");
			for (EObject eob : objlist) {
				System.out.println(eob);
			}
			
			System.out.println();
			System.out.print("Contained Types: ");
			for (Type t : model.getOwnedTypes()) {
				System.out.print(t.toString());
			}
			System.out.println();
			//*/
			structureModel(model);
			System.out.println("Empty?:"+UMLHelper.getAllElementsOfType(model, ActivityPartition.class).isEmpty());
			//
			//------TODO remove
			printContent(model, "");
			for ( Stereotype e : UMLHelper.getAllElementsOfType(model, Stereotype.class)) {
				host.appendLineToReport("---------------------------------||||||||||||||||||------------" + e.toString());
			}
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: "+numOfElements));
			return true;
			//------
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	
	
	
	public void printContent(Element element, String indent) {
		numOfElements++;
		host.appendToReport(indent+element.eClass().getName()+": ");
		if (!element.getAppliedStereotypes().isEmpty()) {
			host.appendToReport("<<");
			for (Stereotype st : element.getAppliedStereotypes()) {
				host.appendToReport(st.getName()+",");
			}
			host.appendToReport(">> ");
		}
		if (element instanceof NamedElement) {
			NamedElement namedElement = (NamedElement)element;
			host.appendToReport(namedElement.getName());
		}
		host.appendLineToReport("");
		for (Element child : element.allOwnedElements()) {
			printContent(child, indent+"  ");
		}
	}
	
	private void structureModel(Package inputModel) {
		Collection<Element> modelContents = inputModel.allOwnedElements();
		//Collection<Element> relevantElements = new LinkedList<Element>();
		Integer idCounter = 0;
		
		for (Element e : modelContents) {
			Collection<Stereotype> stereotypes = e.getAppliedStereotypes();
			System.out.println("stereotypes empty: " + stereotypes.isEmpty());
			for (Stereotype s : stereotypes) {
				System.out.println(s.getProfile().getQualifiedName());
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					System.out.println("is equal");
					processStereotype_creation(s, idCounter);
				}
			}
		}
		/*
		//Filter for Elements stereotyped by the relevant profile
		for (Element e : modelContents) {
			Collection<Stereotype> stereotypes = e.getAppliedStereotypes();
			System.out.println("stereotypes empty: " + stereotypes.isEmpty());
			for (Stereotype s : stereotypes) {
				System.out.println(s.getProfile().getQualifiedName());
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					System.out.println("is equal");
					relevantElements.add(e);
				}
			}
		}
		
		for (Element e : relevantElements) {
			for (Stereotype s : e.getAppliedStereotypes()) {
				System.out.println("Owner of " + s + " : " +s.getOwner());
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					System.out.println("is equal");
					processStereotype_creation(s);
					//e.get
				}
			}
			System.out.println("processing " + e.toString());
			
		}*/
		
		
		
		
		///////////////////
		List<Stereotype> stereotypedElements = UMLHelper.getAllElementsOfType(inputModel, Stereotype.class);
		System.out.println("before the stereotypes");
		for (Stereotype s : stereotypedElements) {
			//System.out.println(s.toString());
		}
		System.out.println("after the stereotypes");
	}
	
	
	private void processElement_creation(Element e) {
		Collection<Stereotype> stereotypes = e.getAppliedStereotypes();
		System.out.println("stereotypes empty: " + stereotypes.isEmpty());
		for (Stereotype s : stereotypes) {
			System.out.println(s.getProfile().getQualifiedName());
			if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
				System.out.println("is equal");
				//processStereotype_creation(s);
			}
		}
	}
	
	private void processStereotype_creation(Stereotype stereotype, Integer id) {
		ExtendedJSONObject currentObject;
		//
		System.out.println("Stereotype name: "+(stereotype.getName()));
		//
		String stereotypeName = stereotype.getName();
		//possibly replace switch-case with if-clauses that check for inheritance from a common superclass (such as Rule for Permission/Prohibition/Obligation
		switch (stereotypeName) {
		case "ODRL-Policy":
			//maybe put the bodies of the cases into own methods
			currentObject = new ExtendedJSONObject();
			currentObject.put("type",stereotypeName);//replace type by a common type-attribute name defined in a class attribute
			odrlObjects.put((id++).toString(), currentObject);
			addToType(currentObject, stereotypeName);			
			break;
		case "Permission":
		case "Prohibition":
		case "Obligation":
			currentObject = new ExtendedJSONObject();
			currentObject.put("type", stereotypeName);
			odrlObjects.put((id++).toString(), currentObject);
			addToType(currentObject, stereotypeName);	
			List<org.eclipse.uml2.uml.Class> baseClasses = stereotype.getAllExtendedMetaclasses();
			for (Property p : stereotype.getAttributes()) {
				//add Attribute to the odrlObjects if attribute value is not simple type and attribute name ist not of pattern base_*ContainedInNamesOfBaseClassesList*
			}
			
			
			break;
		}
	}
	
	
	
	private void addToType(ExtendedJSONObject typedElement, String type) {
		if (!typeBuckets.containsKey(type)) {
			typeBuckets.put(type, new TreeSet<ExtendedJSONObject>());
		}
		typeBuckets.get(type).add(typedElement);	
	}
	

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}

	

}