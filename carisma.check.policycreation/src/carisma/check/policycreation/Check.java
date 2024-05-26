package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Action;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityNode;
import org.eclipse.uml2.uml.ActivityPartition;
import org.eclipse.uml2.uml.DataType;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Feature;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.ProfileApplication;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.StructuralFeature;
import org.eclipse.uml2.uml.Type;
import org.eclipse.uml2.uml.internal.impl.PropertyImpl;
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
	Map<EObject,ExtendedJSONObject> directlyContainedOdrlObjects = new HashMap<EObject, ExtendedJSONObject>();
	Map<String,Collection<ExtendedJSONObject>> typeBuckets = new HashMap<String,Collection<ExtendedJSONObject>>();
	ExtendedJSONObject root;
	//Map<JSONObject>
	final String type = "type";

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
				
				for (Stereotype st : e.getAppliedStereotypes()) {
					System.out.println("Stereotype: "+st);
					if (st.getName().equals("ODRL-Policy")) {
					}
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
			//System.out.println("Instance of model?: " + (model instanceof Model) + "  "+ model.getModel());
			
			structureModel(model);
			
			 //System.out.println("Empty?:"+UMLHelper.getAllElementsOfType(model.getModel(), ActivityPartition.class).isEmpty());
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
		
		for (Element e : modelContents) {
			/*
			
			for (EObject ele: e.getStereotypeApplications()) {
				System.out.println("there's a stereotypeApplication of: " + ele);
				for (EStructuralFeature esf1 : ele.eClass().getEAllStructuralFeatures()) {//alternativ geteallattributes
					System.out.println("With EStructuralfeature:" + esf1.getName() + ": " + ele.eGet(esf1) + " of class " + (ele.eGet(esf1) != null ? ele.eGet(esf1).getClass() : "Null") + " equaling ActivityNode? " + (ele.eGet(esf1) instanceof ActivityNode));
					System.out.println("With EStructuralfeature:" + esf1.getName() + ": " + ele.eGet(esf1) + " of class " + esf1.getEType() + " equaling ActivityNode? " + (esf1.getEType() instanceof ActivityNode)); //klappt nicht
				}
				for (EStructuralFeature esf1 : ele.eClass().getEAllAttributes()) {////////
					System.out.println("With EAttributes: " + esf1.getName() + ": " + ele.eGet(esf1));
				}
				for (EObject eobj : ele.eContents()) {
					System.out.println("Contained EObject: "+ eobj);
				}
				
			}
			
			
			*/
			
			
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					processStereotype_creation(e.getStereotypeApplication(s));
				}
			}
			
			
			/*
			Collection<Stereotype> stereotypes = e.getAppliedStereotypes();
			System.out.println("stereotypes empty: " + stereotypes.isEmpty());
			for (Stereotype s : stereotypes) {
				System.out.println(s.getProfile().getQualifiedName());
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					System.out.println(s.getName());
					if(s.getName().equals("Duty")) {
						System.out.println("Testing Permission---------------------------------------:");
						System.out.println(e.getValue(s, "uid"));
						for (Element el : s.getOwnedElements()) {
							System.out.println("Stereotype owned elements: " + el);
						}
					}
					//
					processStereotype_creation(s);
				}
			}
			*/
			/*
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					EObject stereoAppl = e.getStereotypeApplication(s);
					//TODO processStereotype_creation(stereoAppl);
					for (Property p : s.getAllAttributes()) {
						System.out.println("Value of " + p.getName() + ": " + e.getValue(s, p.getName()) + "    +    " + e.getValue(s, p.getName()).getClass());
						Object elo = e.getValue(s, p.getName());
						if (elo instanceof DataType) {
							DataType dt = (DataType) elo;
							//}
						//for (Object o : e.getValue(s, p.getName()) 
						}
					}
					System.out.println();
				}
			}*/
				/*if (stereoAppl.getProfile().getQualifiedName().equals("ODRLCommonVocabulary")) { //create final-Variable for profile, propably replace qualified name comparison by an actually unique identifier
					System.out.println(s.getName());
					if(s.getName().equals("Duty")) {
						System.out.println("Testing Permission---------------------------------------:");
						System.out.println(e.getValue(s, "uid"));
						for (Element el : s.getOwnedElements()) {
							System.out.println("Stereotype owned elements: " + el);
						}
					}
					//
					processStereotype_creation(s);
				}*/
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
		
		
		
	
	}
	
/*	
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
	*/
	
	private void processStereotype_creation(EObject stereoAppl) {
		ExtendedJSONObject currentObject = new ExtendedJSONObject();
		/*
		System.out.println("Stereotype name: "+(stereotype.getName()));
		System.out.println("Stereotype qualified name: "+(stereotype.getQualifiedName()));
		System.out.println("Stereotype is instance of ODRL-Policy: "+(stereotype.getClass()));
		*/
		//Collection<ExtendedJSONObject> containedObjects = new LinkedList<ExtendedJSONObject>();
		
		String stereotypeName = stereoAppl.eClass().getName();
		
		currentObject.put(type,stereotypeName);
		addToType(currentObject, stereotypeName);//propably chose a more unique id. qualifiedName?	
		directlyContainedOdrlObjects.put(stereoAppl,currentObject);
		System.out.println("now checking for subobjects of " + stereoAppl);
		addSubObjects(stereoAppl, currentObject);
		
		
		
		
		/*
		//possibly replace switch-case with if-clauses that check for inheritance from a common superclass (such as Rule for Permission/Prohibition/Obligation
		switch (stereotypeName) {
		case "ODRL-Policy":
			//maybe put the bodies of the cases into own methods
			currentObject = new ExtendedJSONObject();
			currentObject.put(type,stereotypeName);//replace type by a common type-attribute name defined in a class attribute
			addToType(currentObject, stereotypeName);	
			containedObjects.add(currentObject);				
			break;
		case "Permission":
		case "Prohibition":
		case "Obligation":
			currentObject = new ExtendedJSONObject();
			currentObject.put(type, stereotypeName);
			containedObjects.add(currentObject);
			addToType(currentObject, stereotypeName);
			for (Property p : stereotype.getAttributes()) {
				System.out.println("Property " + p.getName() + ":" + p.eContents());
				
				//if (p.getValue())
				//add Attribute to the odrlObjects if attribute value is simple type, datatype or Enum
			}
			
			
			break;
			
		}
		
		
		//add new Elements to 
		if (!containedObjects.isEmpty()) {
			for(ExtendedJSONObject containedObject : containedObjects) {;
				if (containedObject.get(type) instanceof String) {
					String objectType = (String) containedObject.get(type);
				addToType(containedObject, objectType);
				}
			}
			containedOdrlObjects.put(stereotype, containedObjects);
		}
		*/
	}
	
	public void addSubObjects(EObject currentElementUML, ExtendedJSONObject currentElementJSON) {
		System.out.println("Eclass: " + currentElementUML.eClass() + " instance of Element?: " + (currentElementUML instanceof Element));
		for (EStructuralFeature feature : currentElementUML.eClass().getEAllStructuralFeatures()) {
			Object featureValue = currentElementUML.eGet(feature);
			System.out.println("Now checking type of property " + feature.getName());
			System.out.println(featureValue==null ? "Null" : "FeatureValue: " + featureValue.toString() +"  FeatureValueType: " + featureValue.getClass() + "   FeatureType: " + feature.getEType() + "    Feature: " + feature);
			System.out.println("Instance of Stereotype:  FeatureValue: " + (featureValue instanceof Stereotype) + " " + Stereotype.class.isInstance(featureValue));
			
			if (!(featureValue instanceof ActivityNode) && !(featureValue instanceof Stereotype)) {
				if (featureValue instanceof List) {
					//create method to iteratively check whether an Object is a list containing Stereotypes or ActivityNodes at some depth and use it as boolean in the first instanceof--if-clause
					System.out.println( ((List) featureValue).get(0) instanceof Stereotype);//add emptyCheck
				}
				System.out.println("Non-Node and Non-Stereotype. Value: " + currentElementUML.eGet(feature) + " of class " + (currentElementUML.eGet(feature) != null ? currentElementUML.eGet(feature).getClass() : "Null"));
				//if (featureValue instanceof simp)
				//ExtendedJSONObject newObject;
			}
		}
	}
	
	
	private void addToType(ExtendedJSONObject typedElement, String type) {
		if (!typeBuckets.containsKey(type)) {
			typeBuckets.put(type, new HashSet<ExtendedJSONObject>());
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