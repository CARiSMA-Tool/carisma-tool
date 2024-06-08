package carisma.check.policycreation;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
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
import org.json.JSONObject;

import ODRLCommonVocabulary.Action;
import ODRLCommonVocabulary.AssetRelationType;
import ODRLCommonVocabulary.ConflictStrategy;
import ODRLCommonVocabulary.ConstraintOperator;
import ODRLCommonVocabulary.LeftOperand;
import ODRLCommonVocabulary.LogicalOperator;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.PartyFunctionType;
import ODRLCommonVocabulary.PolicyType;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.AttributedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.CompensatedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.CompensatingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ConsentedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ConsentingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ContractedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.ContractingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.InformedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.InformingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackedParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.function.TrackingParty;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Assertion;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Privacy;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Request;
import carisma.profile.uconcreation.odrl.common.internal.classes.policy.Ticket;
import carisma.profile.uconcreation.odrl.common.internal.classes.relation.Output;
import carisma.profile.uconcreation.odrl.core.internal.classes.ODRLClass;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.Asset;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.AssetCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Permit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Prohibit;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.VoidPolicy;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.Constraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.LogicalConstraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Failure;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assignee;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Assigner;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.And;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.AndSequence;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Operand;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Or;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Xone;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.Operator;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.PartyCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Agreement;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Offer;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Policy;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Set;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Target;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Duty;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Permission;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Prohibition;
import carisma.profile.uconcreation.odrl.core.internal.classes.rule.Rule;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";

	public static final String CHECK_NAME = "Policy Model Transformation";
	
//--------------
	AnalysisHost host;
	Map<EObject,ExtendedJSONObject> referencingList = new HashMap<EObject, ExtendedJSONObject>(); //Would have been used if JSON was used for internal representation
	Map<EObject,ODRLClass> referencingList2 = new HashMap<EObject, ODRLClass>();
	Map<String,Collection<ExtendedJSONObject>> typeBuckets = new HashMap<String,Collection<ExtendedJSONObject>>();
	ExtendedJSONObject root;
	Package usedPackage;
	//Map<JSONObject>
	final String type = "@type";
	final String nullString = "Null";
	final String profileName = "ODRLCommonVocabulary";
	ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;

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
		List<ODRLClass> generatedClasses = new LinkedList<ODRLClass>();
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
					ODRLClass odrlC = (addElement(e.getStereotypeApplication(s), null, e));
					System.out.println(odrlC);
					if (odrlC instanceof Policy policy) {
						System.out.println(new JSONObject(policy));
					}
					if (odrlC instanceof Rule rule ) {
						System.out.println(new JSONObject(rule));
					}
					if (odrlC instanceof Rule rule ) {
						System.out.println(new JSONObject(rule));
					}
					System.out.println();
					//processStereotype_create(e.getStereotypeApplication(s));
				}
				
			}
		}
		/*
		for (ODRLClass oc : generatedClasses) {
			System.out.println(oc);
			System.out.println(new JSONObject(oc));
			System.out.println();
		}
		*/
		
		
		
	
	}
	
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
	private ODRLClass addElement(EObject currentEObject, ODRLClass odrlParent, EObject activityElement) {//TODO modify the names if the eCore naming derives form the names in generated code
		if (referencingList2.get(currentEObject)!= null) {//TODO either add parent to parents-Attribute here (and before the other return-statement) or in the method processing the return value (if that's where it's decided whether it's actually added (for example in case it would, but must not ovewrite a value of the parent))
			return referencingList2.get(currentEObject);
		}
		String objectClassName = currentEObject.eClass().getName();
		ODRLClass newOdrlObject = null;
		EClass autoGenClass = null;
		EStructuralFeature classFeature = null;
		if (currentEObject instanceof EEnumLiteral eEnumLiteralObject) {//ODRLClasses without content
			String objectName = currentEObject.toString();		

			if (objectClassName.equals(ConflictStrategy.class.getSimpleName())){//try to get names (also attribute names (but getting fields by attribute (not String) seems not supported in java) from the genmodel-code to minimize the risk of spelling mistakes
				if (objectName.equals(ConflictStrategy.PERMIT.getName())) {
					newOdrlObject = new Permit();
				}
				else if (objectName.equals(ConflictStrategy.PROHIBIT.getName())) {
					newOdrlObject = new Prohibit();
				}
				else if (objectName.equals(ConflictStrategy.VOID_POLICY.getName())) {
					newOdrlObject = new VoidPolicy();
				}
			}
			else if (objectClassName.equals(PolicyType.class.getSimpleName())) {//TODO Remove (used as type-enum in creation of ODRLPolicy)
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(PartyFunctionType.class.getSimpleName())) {//TODO Remove (used as type-enum in creation of PartyFunction)
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(AssetRelationType.class.getSimpleName())) {//TODO Remove (used as type-enum in creation of AssetRelation)
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(Action.class.getSimpleName())) {
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(LogicalOperator.class.getSimpleName())) {//TODO Remove (used as type-enum in creation of LogicalConstraint)
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(ConstraintOperator.class.getSimpleName())) {
				//if(objectName.equals()) {
				//}
			}
			else if (objectClassName.equals(LeftOperand.class.getSimpleName())) {
				//if(objectName.equals()) {
				//}
			}
		}
		
		//Rules
		else if (objectClassName.equals(odrlPackage.getPermission().getName())) {//Erst Erstellung des jeweiligen Objekts mit den einzelnen Klassen, am Ende dann auf Basis von Vererbung die Attribute füllen (z.B. if instanceof Rule: currentObject.adduid( eObject.getFeature(Rule.getUID().getName()) )
			autoGenClass = odrlPackage.getPermission();//?
			newOdrlObject=new Permission();			
		}
		else if (objectClassName.equals(odrlPackage.getProhibition().getName())) {
			newOdrlObject=new Permission();
		}
		else if (objectClassName.equals(odrlPackage.getDuty().getName())) {
			newOdrlObject=new Duty();
		}
		//Policy (type enum-attribute-determined)
		else if (objectClassName.equals(odrlPackage.getODRLPolicy().getName())) {
			classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_PolicyType().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.getName().equals(PolicyType.AGREEMENT.getName())) {
					newOdrlObject = new Agreement();
				}
				else if (classEnum.getName().equals(PolicyType.ASSERTION.getName())) {
					newOdrlObject = new Assertion();
				}
				else if (classEnum.getName().equals(PolicyType.OFFER.getName())) {
					newOdrlObject = new Offer();
				}
				else if (classEnum.getName().equals(PolicyType.PRIVACY.getName())) {
					newOdrlObject = new Privacy();
				}
				else if (classEnum.getName().equals(PolicyType.REQUEST.getName())) {
					newOdrlObject = new Request();
				}
				else if (classEnum.getName().equals(PolicyType.SET.getName())) {
					newOdrlObject = new Set();
				}
				else if (classEnum.getName().equals(PolicyType.TICKET.getName())) {
					newOdrlObject = new Ticket();
				}
				else {//Default case, no type-information (is interpreted as Set-Policy by evaluators)
					newOdrlObject = new Policy();
				}
			}
			
		}
		//AssetRelation (type enum-attribute-determined)
		else if (objectClassName.equals(odrlPackage.getAssetRelation().getName())) {
			classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getAssetRelation_Type().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.getName().equals(AssetRelationType.TARGET.getName())) {
					newOdrlObject = new Target();
				}
				else if (classEnum.getName().equals(AssetRelationType.OUTPUT.getName())) {
					newOdrlObject = new Output();
				}
			}
		}
		//PartyFunction (type enum-attribute-determined)
		else if (objectClassName.equals(odrlPackage.getPartyFunction().getName())) {
			classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getPartyFunction_Type().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.getName().equals(PartyFunctionType.ASSIGNEE.getName())) {
					newOdrlObject = new Assignee();
				}
				else if (classEnum.getName().equals(PartyFunctionType.ASSIGNER.getName())) {
					newOdrlObject = new Assigner();
				}
				else if (classEnum.getName().equals(PartyFunctionType.ATTRIBUTED_PARTY.getName())) {
					newOdrlObject = new AttributedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.COMPENSATED_PARTY.getName())) {
					newOdrlObject = new CompensatedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.COMPENSATING_PARTY.getName())) {
					newOdrlObject = new CompensatingParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.CONSENTED_PARTY.getName())) {
					newOdrlObject = new ConsentedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.CONSENTING_PARTY.getName())) {
					newOdrlObject = new ConsentingParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.CONTRACTED_PARTY.getName())) {
					newOdrlObject = new ContractedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.CONTRACTING_PARTY.getName())) {
					newOdrlObject = new ContractingParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.INFORMED_PARTY.getName())) {
					newOdrlObject = new InformedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.INFORMING_PARTY.getName())) {
					newOdrlObject = new InformingParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.TRACKED_PARTY.getName())) {
					newOdrlObject = new TrackedParty();
				}
				else if (classEnum.getName().equals(PartyFunctionType.TRACKING_PARTY.getName())) {
					newOdrlObject = new TrackingParty();
				}
			}
		}
		//LogicalOperators (type enum-attribute-determined)
		else if (objectClassName.equals(odrlPackage.getLogicalConstraint().getName())) {
			classFeature = currentEObject.eClass().getEStructuralFeature(odrlPackage.getPartyFunction_Type().getName());
			if (currentEObject.eGet(classFeature) instanceof EEnumLiteral classEnum) {
				if (classEnum.getName().equals(LogicalOperator.AND.getName())) {
					newOdrlObject = new And();
				}
				else if (classEnum.getName().equals(LogicalOperator.AND_SEQUENCE.getName())) {
					newOdrlObject = new AndSequence();
				}
				else if (classEnum.getName().equals(LogicalOperator.OR.getName())) {
					newOdrlObject = new Or();
				}
				else if (classEnum.getName().equals(LogicalOperator.XONE.getName())) {
					newOdrlObject = new Xone();
				}
				else if (classEnum.getName().equals(LogicalOperator.NULL.getName())) {//LogicalConstraint only used as wrapper for the constraint as using a common supertype to make both eligible as value does not work with papyrus 
					newOdrlObject = new Constraint();
				}
			}
		}
		/*
		//
		//Fill attributes of generated classes
		//
		if (newOdrlObject instanceof Policy newPolicy) {
			//conflict
			Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_ConflictStrategy().getName()));
			if (attributeValue instanceof EObject newEObj) {
				ODRLClass attributeValueOdrl = addElement(newEObj, newOdrlObject);
				if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy conflictValue) {
					newPolicy.setConflictStrategy(conflictValue);
				}
			}
			//
			attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_InheritsFrom().getName()));
			if (attributeValue instanceof EObject newEObj) {//TODO String List attribute				
			}
			attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_Profiles().getName()));
			if (attributeValue instanceof EObject newEObj) {//TODO String List attribute
			}
			attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_Uid().getName()));
			if (attributeValue instanceof EObject newEObj) {
				ODRLClass attributeValueOdrl = addElement(newEObj, newOdrlObject);
				if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy conflictValue) {
					newPolicy.setConflictStrategy(conflictValue);
				}
			}
		}
		*/
		//Filling the generated object
		if (newOdrlObject instanceof Asset asset) {
			fillAsset(currentEObject, asset, activityElement);
		}
		if (newOdrlObject instanceof AssetCollection assetCollection) {
			fillAssetCollection(currentEObject, assetCollection, activityElement);
		}
		if (newOdrlObject instanceof Constraint constraint) {
			fillConstraint(currentEObject, constraint, activityElement);
		}
		if (newOdrlObject instanceof  Failure failure) {
			fillFailure(currentEObject, failure, activityElement);
		}
		if (newOdrlObject instanceof  Function function) {
			fillFunction(currentEObject, function, activityElement);
		}
		//TODO add rest
		if (newOdrlObject instanceof  Policy policy) {
			fillPolicy(currentEObject, policy, activityElement);
		}
		if (newOdrlObject instanceof  Rule rule) {
			fillRule(currentEObject, rule, activityElement);
		}
		
		
		
		if (newOdrlObject!=null) {
			referencingList2.put(currentEObject, newOdrlObject);
		}
		return newOdrlObject;
	}
	
	private boolean addElement(EObject eParent, EStructuralFeature feature, ODRLClass odrlParent) {
		Object eValue = eParent.eGet(feature);
		//if (eParent instanceof )
			
			return false;
		return false;
	}
	
	//Filling-Methods for assets
	private void fillAsset(EObject currentEObject, Asset asset, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getAsset_Uid().getName()));
		if (attributeValue instanceof String stringValue) {
			asset.setUid(stringValue);
		}
	}
	private void fillAssetCollection(EObject currentEObject, AssetCollection assetCollection, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getAssetCollection_Source().getName()));
		if (attributeValue instanceof String stringValue) {
			assetCollection.setSource(stringValue);
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRefinableElement_Refinement().getName())); 
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, assetCollection, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface refinement) {
				assetCollection.setRefinement(refinement);
			}
		}
	}
	//Filling-Methods for Conflict (TODO only maybe add (as the methods would be empty))
	//Filling-Methods for Constraints (TODO maybe add ConstraintInterface)
	private void fillConstraint(EObject currentEObject, Constraint constraint, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_DataType().getName()));
		if (attributeValue instanceof String stringValue) {
			constraint.setDataType(stringValue);
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_LeftOperand().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, constraint, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.leftoperand.LeftOperand leftOperand) {
				constraint.setLeftOperand(leftOperand);
			}
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_Operator().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, constraint, activityElement);
			if (attributeValueOdrl instanceof Operator operator) {
				constraint.setOperator(operator);
			}
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_RightOperand().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_RightOperandReference().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_Status().getName()));
		if (attributeValue instanceof String stringValue) {
			constraint.setStatus(stringValue);
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_Uid().getName()));
		if (attributeValue instanceof String stringValue) {
			constraint.setUid(stringValue);
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getConstraint_Unit().getName()));
		if (attributeValue instanceof String stringValue) {
			constraint.setUnit(stringValue);
		}
	}
	private void fillLogicalConstraint(EObject currentEObject, LogicalConstraint logicalConstraint, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_Constraints().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_Uid().getName()));
		if (attributeValue instanceof String stringValue) {
			logicalConstraint.setUid(stringValue);
		}
	}
	//Filling-Methods for Failures (TODO maybe add empty subproperties)
	private void fillFailure(EObject currentEObject, Failure failure, EObject activityElement) {//TODO failure currently not present on the uml-profile
		//Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage));
		//if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		//}
	}
	//Filling-Methods for Functions
	private void fillFunction(EObject currentEObject, Function function, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getPartyFunction_Party().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, function, activityElement);
			if (attributeValueOdrl instanceof Party party) {
				function.setParty(party);
			}
		}
		
	}
	//Filling-Methods for Leftoperands (TODO only maybe add (as the methods would be empty))
	//Filling-Methods for Operands (TODO maybe add empty subproperties)
	private void fillOperand(EObject currentEObject, Operand operand, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getLogicalConstraint_Constraints().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO List attribute
		}
	}
	//Filling-Methods for Operators (TODO only maybe add (as the methods would be empty))
	//Filling-Methods for Parties
	private void fillParty(EObject currentEObject, Party party, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getParty_Uid().getName()));
		if (attributeValue instanceof String stringValue) {
			party.setUid(stringValue);
		}
	}
	private void fillPartyCollection(EObject currentEObject, PartyCollection partyCollection, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getPartyCollection_Source().getName()));
		if (attributeValue instanceof String stringValue) {
			partyCollection.setSource(stringValue);
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRefinableElement_Refinement().getName())); 
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, partyCollection, activityElement);
			if (attributeValueOdrl instanceof ConstraintInterface refinement) {
				partyCollection.setRefinement(refinement);
			}
		}
	}	
	//Filling-methods for Policies (TODO maybe add empty subclass-methods)
	private void fillPolicy(EObject currentEObject, Policy policy, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_ConflictStrategy().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, policy, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.conflict.ConflictStrategy conflictValue) {
				policy.setConflictStrategy(conflictValue);
			}
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_InheritsFrom().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO String List attribute				
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_Profiles().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO String List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getODRLPolicy_Uid().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO String List attribute
		}
	}
	//Filling-Methods for Relation
	private void fillRelation(EObject currentEObject, Relation relation, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getAssetRelation_Asset().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, relation, activityElement);
			if (attributeValueOdrl instanceof Asset asset) {
				relation.setAsset(asset);
			}
		}
	}
	//Filling-Methods for RightOperands TODO deal with once the different RightOperandInterface-implementers are finished	
	//Filling-methods for rules
	private void fillRule(EObject currentEObject, Rule rule, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRule_Action().getName()));
		if (attributeValue instanceof EObject newEObj) {
			ODRLClass attributeValueOdrl = addElement(newEObj, rule, activityElement);
			if (attributeValueOdrl instanceof carisma.profile.uconcreation.odrl.core.internal.classes.action.Action action) {
				rule.setAction(action);
			}
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRule_Uid().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO String List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRule_InvolvedAssets().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO List attribute
		}
		attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getRule_InvolvedParties().getName()));
		if (attributeValue instanceof EObject newEObj) {//TODO List attribute
		}		
	}
	private void fillPermission(EObject currentEObject, Permission permission, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getPermission_Duties().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
	}
	private void fillProhibition(EObject currentEObject, Prohibition prohibition, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getProhibition_Remedies().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
	}
	private void fillDuty(EObject currentEObject, Duty duty, EObject activityElement) {
		Object attributeValue = currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getDuty_Consequences().getName()));
		if (attributeValue instanceof EObject newEObj) { //TODO List attribute
		}
	}
	
	//Maybe (for brevity) replace all occurrences of currentEObject.eGet(currentEObject.eClass().getEStructuralFeature(odrlPackage.getX_Y().getName())) with getEObjValue(currentObject, odrlPackage.getX_Y().getName()
	private Object getEObjValue(EObject eObject, String featureName) {
		Object attributeValue = eObject.eGet(eObject.eClass().getEStructuralFeature(featureName));
		return attributeValue;
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