package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EEnumLiteral;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.impl.EClassImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.ActivityContent;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.json.JSONArray;

import ODRLCommonVocabulary.ConflictStrategy;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Permit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Prohibit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.VoidPolicy;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";

	public static final String CHECK_NAME = "Policy Model Transformation";
	
//--------------
	AnalysisHost host;
	Map<EObject,ExtendedJSONObject> referencingList = new HashMap<EObject, ExtendedJSONObject>();
	Map<String,Collection<ExtendedJSONObject>> typeBuckets = new HashMap<String,Collection<ExtendedJSONObject>>();
	ExtendedJSONObject root;
	Package usedPackage;
	//Map<JSONObject>
	final String type = "@type";
	final String nullString = "Null";
	final String profileName = "ODRLCommonVocabulary";

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
			usedPackage = model;
			for (Stereotype s : usedPackage.getAppliedProfile(profileName).getOwnedStereotypes())
			System.out.println("-----------------Owned Stereotype: " + s);
			for (NamedElement ne : usedPackage.getAppliedProfile(profileName).getOwnedMembers())
				System.out.println("-------|||--------Named Element: " + ne + " --------||||------- " + ne.eClass());
			//
			
			//
			for (Element e : model.allOwnedElements()) {
				System.out.println("Owned Element: " + e);
				
				for (Stereotype st : e.getAppliedStereotypes()) {
					System.out.println("Stereotype: "+st);
					if (st.getName().equals("ODRL-Policy")) {
					}
					if (st.getQualifiedName().equals(profileName + "::Permission")) {
						if (e.getValue(st, "refinement") instanceof EObject) {
							EObject datatype = (EObject) e.getValue(st, "refinement");
							System.out.println("           -------------             "+ datatype);
							
							for (EStructuralFeature f : datatype.eClass().getEAllStructuralFeatures()) {
								System.out.println(f);
								System.out.println(datatype.eGet(f));
								System.out.println(datatype.eGet(f).getClass());
						}
							System.out.println("-|-|-|-<");
						}
					}
				}
			}
			for (NamedElement ne : model.getMembers())
				System.out.println(ne.getName() + "      " + ne);
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
				if (s.getProfile().getQualifiedName().equals(profileName)) { //propably replace qualified name comparison by an more unique identifier
					processStereotype_create(e.getStereotypeApplication(s));
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
	
	private void processStereotype_create(EObject stereoAppl) {
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
		if (!referencingList.containsKey(stereoAppl)) {
			referencingList.put(stereoAppl,currentObject);
			System.out.println("now checking for subobjects of " + stereoAppl);
			addSubObjects_create(stereoAppl, currentObject);
		}
		
		
		System.out.println(currentObject.toString(3));
		
		
		System.out.println("All Trees:");
		for (ExtendedJSONObject eJO: referencingList.values())
			System.out.println(eJO.toString(5));
		
		
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
	
	public void addSubObjects_create(EObject currentElementUML, ExtendedJSONObject currentElementJSON) {
		
		System.out.println("Eclass: " + currentElementUML.eClass() + " instance of Element?: " + (currentElementUML instanceof Element) + "  " + (Element.class.isInstance(currentElementJSON)));
		//for ()
		
		
		for (EStructuralFeature feature : currentElementUML.eClass().getEAllStructuralFeatures()) {			
			Object featureValue = currentElementUML.eGet(feature);
			/*if (featureValue instanceof EObject) {
				Element profileElement = usedPackage.getAppliedProfile(profileName).getOwnedMember(((EObject)featureValue).eClass().getName());
				if (profileElement instanceof Stereotype || featureValue instanceof ActivityNode) {
					continue;
				}
			}*/
			System.out.println("FeatureValue of " + feature.getName() + " in addSubObjects: " + featureValue + "  |||  Feature: " + feature + "   |||   Feature.getClass()" + feature.getClass() + " |||  Feature.eClass(): " + feature.eClass() + " ||| Feature.getEType(): " + feature.getEType());
			System.out.println((feature.getEType() instanceof EEnum) + "    " + (feature.getEType() instanceof EDataType) + "     " + (feature.getEType() instanceof EClassImpl));	
			//EEnum, EDatatype, EClassImpl, value auf Element prüfen
			addInFirstStep2(currentElementUML, feature, currentElementJSON);
				
			
			/*
			System.out.println("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
			System.out.println("Now checking type of property " + feature.getName());
			System.out.println("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
			System.out.println(featureValue==null ? "Null" : "FeatureValue: " + featureValue.toString() +"  FeatureValueType: " + featureValue.getClass() + "   FeatureType: " + feature.getEType() + "    Feature: " + feature);
			System.out.println("Instance of Element:  FeatureValue: " + (featureValue instanceof Element) + " " + Element.class.isInstance(featureValue));
			System.out.println("instance of ActivityNode:" + (featureValue instanceof ActivityNode) + ",   instance of Stereotype: " + (featureValue instanceof Stereotype));
			if (!(featureValue instanceof ActivityNode) && !(featureValue instanceof Stereotype)) {
				if (featureValue instanceof List) {
					//create method to iteratively check whether an Object is a list containing Stereotypes or ActivityNodes at some depth and use it as boolean in the first instanceof--if-clause
					if (!((List) featureValue).isEmpty()) {
					}
				}
				//
				//if (featureValue instanceof EObject && !(feature.getName().contains("base_"))) {
				//addSubObjects(((EObject)featureValue), currentElementJSON);
				//}
				//
				System.out.println("Non-Node and Non-Stereotype. Value: " + currentElementUML.eGet(feature) + " of class " + (currentElementUML.eGet(feature) != null ? currentElementUML.eGet(feature).getClass() : "Null"));
				//if (featureValue instanceof simp)
				//ExtendedJSONObject newObject;
			}*/
		}
		
		/*
		for (EStructuralFeature feature : currentElementUML.eClass().getEAllStructuralFeatures()) {
			Object featureValue = currentElementUML.eGet(feature);
			System.out.println("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
			System.out.println("Now checking type of property " + feature.getName());
			System.out.println("|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||");
			System.out.println(featureValue==null ? "Null" : "FeatureValue: " + featureValue.toString() +"  FeatureValueType: " + featureValue.getClass() + "   FeatureType: " + feature.getEType() + "    Feature: " + feature);
			System.out.println("Instance of Element:  FeatureValue: " + (featureValue instanceof Element) + " " + Element.class.isInstance(featureValue));
			System.out.println("instance of ActivityNode:" + (featureValue instanceof ActivityNode) + ",   instance of Stereotype: " + (featureValue instanceof Stereotype));
			if (!(featureValue instanceof ActivityNode) && !(featureValue instanceof Stereotype)) {
				if (featureValue instanceof List) {
					//create method to iteratively check whether an Object is a list containing Stereotypes or ActivityNodes at some depth and use it as boolean in the first instanceof--if-clause
					if (!((List) featureValue).isEmpty()) {
						if (((List) featureValue).get(0) instanceof EObject) {
							EObject testeobj = (EObject) ((List) featureValue).get(0);
							
							System.out.println(testeobj.getClass() + "     " + testeobj.eClass() + "        " + testeobj.eClass().eClass());
							System.out.println("------------------");
							for (EStructuralFeature esf : testeobj.eClass().getEAllStructuralFeatures()) {
								System.out.println(esf.getName() + ": " + esf + ",    Value: " + testeobj.eGet(esf));	
								if(esf instanceof EReference)
									System.out.println("EreferenceType: " + ((EReference) esf).getEReferenceType());
								if (esf instanceof EAttribute)
									System.out.println("EAttributeType: " + ((EAttribute) esf).getEAttributeType());
							}
							
							System.out.println("----------------");
							for (EObject eobje : testeobj.eCrossReferences()) {
								System.out.println(eobje + ",          eClass: " + eobje.eClass() + ",    Class: " + eobje.getClass());
							}
							System.out.println("----------------");
							System.out.println(testeobj);//add emptyCheck

							
						}
					}
				}
				//
				//if (featureValue instanceof EObject && !(feature.getName().contains("base_"))) {
				//addSubObjects(((EObject)featureValue), currentElementJSON);
				//}
				//
				System.out.println("Non-Node and Non-Stereotype. Value: " + currentElementUML.eGet(feature) + " of class " + (currentElementUML.eGet(feature) != null ? currentElementUML.eGet(feature).getClass() : "Null"));
				//if (featureValue instanceof simp)
				//ExtendedJSONObject newObject;
			}
		}*/
	}
	
	public boolean addInFirstStep2(EObject currentTop, EStructuralFeature feature, ExtendedJSONObject jObject) {
		if (currentTop.eGet(feature) == null)
			return false;
		//Keeps out the base_Element-attributes of the stereotypes
		if (currentTop.eGet(feature) instanceof ActivityContent || currentTop.eGet(feature) instanceof Activity)
			return false;
		if (currentTop.eGet(feature) instanceof List list) {
			JSONArray addedJson = new JSONArray();
			boolean empty = true;
			
			for (Object listObject : list) {
				 if (addInFirstStep2(listObject, addedJson))
					 empty = false;
			}
			if (!empty) {
				jObject.put(feature.getName(), addedJson);
			}	
			return !empty;
		}
		else if (feature.getEType() instanceof EEnum) {
			System.out.println("EEnum value: " + currentTop.eGet(feature) + "       Class:" + currentTop.eGet(feature).getClass());
			ExtendedJSONObject addedJson = new ExtendedJSONObject();
			addedJson.put(type, currentTop.eGet(feature).toString());
			jObject.put(feature.getName(), addedJson);
			return true;
		}
		else if (feature.getEType() instanceof EDataType) {
			System.out.println("value instance of EDataType in 1: " + feature + "    Etype: " + feature.getEType() + "     Value: " + currentTop.eGet(feature)  + ",      Null?: " + currentTop.eGet(feature)==null);
			if (feature.getEType().getName().equals("String")) {
				jObject.put(feature.getName(), currentTop.eGet(feature).toString());
			}
		} else if (feature instanceof EReference) {
					ExtendedJSONObject addedJson = new ExtendedJSONObject();
					if (currentTop.eGet(feature) instanceof EObject) {
						EObject currentValueEO = (EObject) currentTop.eGet(feature);
						//Referenced Element not already processed
						if (referencingList.get(currentValueEO)==null) {
							addedJson.put(type, feature.getEType().getName());
							referencingList.put(currentValueEO, addedJson);
							for (EStructuralFeature newFeature : ((EObject)(currentTop.eGet(feature))).eClass().getEAllStructuralFeatures() )
								addInFirstStep2(currentValueEO,newFeature, addedJson);
						} else {//Referenced Element already processed
							addedJson = referencingList.get(currentTop.eGet(feature));
					}
					jObject.put(feature.getName(), addedJson);
					return true;
				
					
			}
		}
		return false;
	}
	
	
	
	
	
	
	
	
	public boolean addInFirstStep2(Object value, JSONArray jArray) {
		System.out.println("Processing " + value);
			
		if (value == null) {
			System.out.println("null");
			return false;
		}
		//Keeps out the base_Element-attributes of the stereotypes
		if (value instanceof ActivityContent || value instanceof Activity) {
			return false;
		}
		/*
		 * Not needed with the current Model
		if (value instanceof Collection) {
			JSONArray addedJson = new JSONArray();
			boolean empty = true;
			//TODO: nested List
			
			for (Object listObject : (Collection) currentTop.eGet(feature)) {
				 //if (addInFirstStep(listObject, addedJson))
					 empty = false;
			}
			if (!empty) {
				jObject.put(feature.getName(), addedJson);
			}			
			return !empty;
		}
		else*/
		if (value instanceof EEnum) {
			System.out.println(0);
			ExtendedJSONObject addedJson = new ExtendedJSONObject();
			addedJson.put(type, value.toString());
			jArray.put(addedJson);
			return true;
		}
		else if (value instanceof EDataType) {
			System.out.println("value instance of EDataType in 2: " + value);
			if (((EDataType) value).getName().equals("String")) {
				jArray.put(value.toString());
				return true;
			}
		} else if (value instanceof EObject) {
			EObject valueEOb = (EObject) value;	
			ExtendedJSONObject addedJson = new ExtendedJSONObject();
			//Referenced Element not already processed
			if (referencingList.get(valueEOb)==null) {
				addedJson.put(type, valueEOb.eClass().getName());
				referencingList.put(valueEOb, addedJson);
				for (EStructuralFeature newFeature : (valueEOb).eClass().getEAllStructuralFeatures() )
					addInFirstStep2(valueEOb,newFeature, addedJson);
			} else {//Referenced Element already processed
				addedJson = referencingList.get(valueEOb);
			}		
				
				jArray.put(addedJson);
				return true;	
		}
		return false;
	}
	
	
	
	
	private void addToType(ExtendedJSONObject typedElement, String type) {
		if (!typeBuckets.containsKey(type)) {
			typeBuckets.put(type, new HashSet<ExtendedJSONObject>());
		}
		typeBuckets.get(type).add(typedElement);	
	}
	
	///////////////////////////////
	private ODRLClass addElement(EObject eObject, ODRLClass odrlParent) {//TODO modify the names if the eCore naming derives form the names in generated code
		String objectClassName = eObject.eClass().getName();
		ODRLClass newOdrlObject = null;
		if (eObject instanceof EEnumLiteral eEnumLiteralObject) {
			String objectName = eObject.toString();		

			if (objectClassName.equals(ConflictStrategy.class.getSimpleName())){//try to get names (also attribute names (but getting fields by attribute (not String) seems not supported in java) from the genmodel to minimize the risk of spelling mistakes
				if (objectName.equals(ConflictStrategy.PERMIT.getName())) {
					newOdrlObject = new Permit();
				}
				else if (objectName.equals(ConflictStrategy.PROHIBIT.getName())) {
					newOdrlObject = new Prohibit();
				}
				else if (objectName.equals(ConflictStrategy.VOID_POLICY.getName())) {
					newOdrlObject = new VoidPolicy();
				}
				return newOdrlObject;
			}
		}
			
			return null;
		return null;
	}
	
	private boolean addElement(EObject eParent, EStructuralFeature feature, ODRLClass odrlParent) {
		Object eValue = eParent.eGet(feature);
		//if (eParent instanceof )
			
			return false;
		return false;
	}
	
	
	
	
	
	///////////////////////////////
	
	
	

	@Override
	public String getCheckID() {
		return CHECK_ID;
	}

	@Override
	public String getName() {
		return CHECK_NAME;
	}

	

}