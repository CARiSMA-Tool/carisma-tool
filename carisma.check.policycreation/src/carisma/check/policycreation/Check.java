package carisma.check.policycreation;

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.json.JSONObject;

import ODRLCommonVocabulary.ODRLCommonVocabularyFactory;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import ODRLCommonVocabulary.util.ODRLCommonVocabularySwitchImpl;
import carisma.check.policycreation.profileimpl.core.ODRLClassImpl;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.Asset;
import carisma.profile.uconcreation.odrl.core.internal.classes.asset.AssetCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.conflict.Prohibit;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.Constraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.ConstraintInterface;
import carisma.profile.uconcreation.odrl.core.internal.classes.constraint.LogicalConstraint;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Failure;
import carisma.profile.uconcreation.odrl.core.internal.classes.failure.Remedy;
import carisma.profile.uconcreation.odrl.core.internal.classes.function.Function;
import carisma.profile.uconcreation.odrl.core.internal.classes.operand.Operand;
import carisma.profile.uconcreation.odrl.core.internal.classes.operator.Operator;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.Party;
import carisma.profile.uconcreation.odrl.core.internal.classes.party.PartyCollection;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Agreement;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Offer;
import carisma.profile.uconcreation.odrl.core.internal.classes.policy.Policy;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Relation;
import carisma.profile.uconcreation.odrl.core.internal.classes.relation.Target;
import carisma.profile.uconcreation.odrl.core.internal.classes.rightoperand.RightOperandInterface;
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

	Map<EObject,ODRLClassImpl> referencingList2 = new HashMap<>();
	Map<String,Collection<ODRLClassImpl>> typeBuckets = new HashMap<String,Collection<ODRLClassImpl>>();//TODO Not used anymore
	ODRLClassImpl root;
	Package usedPackage;
	//Map<JSONObject>
	final static String typeString = "@type";
	final static String nullString = "Null";
	//possibly add emptyString-Variable since empty strings are treated as Null-Strings
	final static String profileName = "ODRLCommonVocabulary";
	ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	ODRLCommonVocabularySwitchImpl<? extends ODRLClassImpl> odrlSwitch = new ODRLCommonVocabularySwitchImpl<>();//TODO:remove
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
//		for (EObject content : currentModel.getContents()) {
//			System.out.println("Model content:" + content);
//		}

		if (currentModel.getContents().get(0) instanceof Package model) {
			
			
			//
//			usedPackage = model;
//			for (Stereotype s : usedPackage.getAppliedProfile(profileName).getOwnedStereotypes())
//			System.out.println("-----------------Owned Stereotype: " + s);
//			for (NamedElement ne : usedPackage.getAppliedProfile(profileName).getOwnedMembers())
//				System.out.println("-------|||--------Named Element: " + ne + " --------||||------- " + ne.eClass());
			//
			
			//
//			for (Element e : model.allOwnedElements()) {
//				System.out.println("Owned Element: " + e);
//				
//				for (Stereotype st : e.getAppliedStereotypes()) {
//					System.out.println("Stereotype: "+st);
//					if (st.getName().equals("ODRL-Policy")) {
//					}
//					if (st.getQualifiedName().equals(profileName + "::Permission")) {
//						if (e.getValue(st, "refinement") instanceof EObject) {
//							EObject datatype = (EObject) e.getValue(st, "refinement");
//							System.out.println("           -------------             "+ datatype);
//							
//							for (EStructuralFeature f : datatype.eClass().getEAllStructuralFeatures()) {
//								System.out.println(f);
//								Object value = datatype.eGet(f);
//								if (value != null) {
//								System.out.println(datatype.eGet(f));
//								System.out.println(datatype.eGet(f).getClass());
//								}
//						}
//							System.out.println("-|-|-|-<");
//						}
//					}
//				}
//			}
//			for (NamedElement ne : model.getMembers()) {
//				System.out.println(ne.getName() + "      " + ne);
//			}

			//Convert the UML-Model to the Java-Class-Model used here
			ODRLCommonVocabularyFactory factory = ODRLCommonVocabularyFactory.eINSTANCE;
			System.out.println("Registered package: " + EPackage.Registry.INSTANCE.keySet());
			System.out.println("Test of duty: " + factory.eINSTANCE.createDuty().eClass().getEPackage());
			System.out.println("Package: " + odrlPackage);
//			for (Element e : model.allOwnedElements()) {
//				odrlSwitch.doSwitch(e);//TODO remove
//				System.out.println("Classifier id: " + e.eClass().getClassifierID());
//				System.out.println("InstanceClassName: "+ e.eClass().getInstanceClassName() + "           , InstanenceType: " + e.eClass().getInstanceTypeName());
//			}
			structureModel(model);
			//

			printContent(model, "");

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
		List<JSONObject> printList = new LinkedList<JSONObject>();
		List<ODRLClassImpl> objectList = new LinkedList<ODRLClassImpl>();
		Collection<Element> modelContents = inputModel.allOwnedElements();
		for (Element e : modelContents) {
			System.out.println("Element: " + e);//TODO: remove
			for(EStructuralFeature esf : e.eClass().getEAllStructuralFeatures()) {//TODO remove
				if (e.eGet(esf)!=null) {
					if (e.eGet(esf) instanceof List list && !list.isEmpty())
					System.out.println("Feature: " + esf + "        With value: " + e.eGet(esf));
				}
			}
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals(profileName)) { //probably replace qualified name comparison by an more unique identifier
					Object object = (addElement(e.getStereotypeApplication(s), null, e));
					
					if (object instanceof ODRLClassImpl odrlC) {
						System.out.println("Created ODRLObject: " + odrlC);
						JSONObject jso = new JSONObject(odrlC);
						printList.add(jso);
						objectList.add(odrlC);
						System.out.println(jso.toString(4));
					}
					else {
						System.out.println("Null: " + e.getStereotypeApplication(s));
						for (EStructuralFeature esf : e.getStereotypeApplication(s).eClass().getEStructuralFeatures()) {
							System.out.println("ESF: " + esf);
						}
					}
					System.out.println();
				}			
			}
		}
		for (ODRLClassImpl odrlc : objectList) {
			checkProfileRequirements(odrlc);
		}
//		for (JSONObject jobj : printList) {
//			System.out.println(jobj.toString(4));
//			System.out.println();
//		}
	}
		
		
		
	
	
	
	///////////////////////////////
	private Object addElement(EObject currentEObject, ODRLClassImpl odrlParent, EObject activityElement) {//TODO modify the names if the eCore naming derives form the names in generated code
//		odrlSwitch.doSwitch(currentEObject);//TODO remove
//		System.out.println("Classifier id: " + currentEObject.eClass().getClassifierID());//TODO remove
		if (currentEObject==null) {//Not necessary for result
			return null;
		}
		if (referencingList2.get(currentEObject)!= null) {//TODO either add parent to parents-Attribute here (and before the other return-statement) or in the method processing the return value (if that's where it's decided whether it's actually added (for example in case it would, but must not ovewrite a value of the parent))
			System.out.println("already generated: " + currentEObject);
			return referencingList2.get(currentEObject);//Problem: Same enums are used in the generation of different ODRL-Objects (e.g. one action-enum with several different refinements)
		}
		String objectClassName = currentEObject.eClass().getName();
		Object newObject = null;
		
		UMLModelConverter converter = new UMLModelConverter();
		//Filling the generated object
		newObject = converter.addElement(currentEObject, odrlParent, activityElement);
		//fill(currentEObject, newObject, activityElement);//TODO maybe return boolean with the fill()-method to signal whether it was filled sufficiently
		
		
		//
		if (newObject instanceof ODRLClassImpl newOdrlObject) {
			System.out.println("At end of addElement :" + newObject);
			referencingList2.put(currentEObject, newOdrlObject);//Maybe extend the valid keys and add all objects, not just ODRLClassImples
		}
		System.out.println(converter.addElement(currentEObject,null,null)==null?"Helper: Null": "Helper:  " + converter.addElement(currentEObject,null,null));
		System.out.println("passed EObject: " + currentEObject);
		return newObject;
	}
	
	
	private Object foo() {
		Map<String,Class> map = new HashMap<>();
		map.put(odrlPackage.getProhibition().getName(), Prohibit.class);
		return map;
	}
	
	
	private <T> List<T> addElement(List currentList, ODRLClassImpl odrlParent, EObject activityElement, Class<T> type) {//No check for several layers of lists as that case does not occur in the current model
		List<T> newOdrlList = new LinkedList<>();
		boolean fullyCompartible = true;
		if (currentList!=null && !currentList.isEmpty()) {
			for (Object o : currentList) {
				if (o instanceof EObject eObj) {
					Object newOdrlObject = addElement(eObj,odrlParent,activityElement);
					if (type.isInstance(newOdrlObject)) {
						newOdrlList.add((T)newOdrlObject);
					} else {
						fullyCompartible=false;
					}
				}
				else if (o instanceof String string) {
					Object newOdrlObject = addElement(string,odrlParent,activityElement);
					if (type.isInstance(newOdrlObject)) {
						newOdrlList.add((T)newOdrlObject);
					} else {
						fullyCompartible=false;
					}
				}
			}
		}
		return newOdrlList.isEmpty()||!fullyCompartible? null : newOdrlList;//Only return a List if all elements of the passed List were of the specified class (and it's not empty)
	}

	
	/*
	private boolean addElement(EObject eParent, EStructuralFeature feature, ODRLClassImpl odrlParent) {
		Object eValue = eParent.eGet(feature);
		//if (eParent instanceof )
			
			return false;
		return false;
	}
	*/
	
	private String addElement(String currentObject, ODRLClassImpl odrlParent, EObject activityElement) {
		return currentObject;
	}
	
	
	private Object getValue(EObject eObject, String featureName) {
//		EStructuralFeature feature = eObject.eClass().getEStructuralFeature(featureName);
//		if(feature == null) {
//			//TODO add missing feature information?
//		}
//		return feature==null ? null : eObject.eGet(feature);
		return eObject.eGet(eObject.eClass().getEStructuralFeature(featureName));//Nullpointer-exception with null-feature can only be produced by code errors, not by input errors
	}
	
	
	///////////////////////////////
	
	private void checkProfileRequirements(ODRLClassImpl testedElement) {//replace testedElement with the created objects
		//TODO no checks for valid form of IRIs so far
		if (testedElement instanceof Policy policy) {
			if (policy.getPermission().isEmpty()
					&& policy.getProhibition().isEmpty()
					&& policy.getObligation().isEmpty()) {
				//TODO add warning: invalid policy: needs to have at least one permission, prohibition or obligation
				addWarning("Invalid Policy. Policy needs to have at least one permission, prohibition or obligation.",testedElement);;
			}
			
			if (policy instanceof Offer offer) {
				//TODO one assigner (only one? needed with every rule or just with one?)
			} else if (policy instanceof Agreement agreement) {
				//TODO one assigner, one assignee
			}
		} else if (testedElement instanceof AssetCollection assetCollection) {
			if (assetCollection.getRefinement()!=null
					&& assetCollection.getSource()==null) {
				addWarning("Invalid assetCollection: source-property needs to be used with refinement.", testedElement);
			}
		} else if (testedElement instanceof PartyCollection partyCollection) {
			if (partyCollection.getRefinement()!=null
					&& partyCollection.getSource()==null) {
				addWarning("Invalid partyCollection: source-property needs to be used with refinement.", testedElement);
			}
		} else if (testedElement instanceof Constraint constraint) {
			if (constraint.getLeftOperand()==null) {
				addWarning("Invalid constraint: needs to have a leftOperand selected.",testedElement);
			}
			if (constraint.getOperator()==null) {
				addWarning("Invalid constraint: needs to have an operator selected.",testedElement);
			}
			if ( (constraint.getRightOperand()==null
					||constraint.getRightOperand().isEmpty() 
					) && ( constraint.getRightOperandReference()==null
					||constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: needs to have either a rightOperand, or rightOperandReference, has neither.",testedElement);
			}
			if ( (constraint.getRightOperand()!=null
					&& !constraint.getRightOperand().isEmpty() 
					 && constraint.getRightOperandReference()!=null
					&& !constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: must not have both rightOperand and rightOperandReference (is that the case?)",testedElement);//TODO check if restriction actually exists
			}
		}
		if (testedElement instanceof Rule rule) {
			if (rule.getAction() == null) {
				addWarning("Invalid rule: needs to have an action selected.",testedElement);
			}
			if (testedElement instanceof Permission permission) {
				boolean hasTarget = false;
				for (Relation relation : permission.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid permission: needs to have a relation of type target.",testedElement);
				}
			} else if (testedElement instanceof Prohibition prohibition) {
				boolean hasTarget = false;
				for (Relation relation : prohibition.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid prohibition: needs to have a relation of type target.",testedElement);
				}
				if (prohibition.getRemedy()!=null && prohibition.getRemedy().getRules()!=null && !prohibition.getRemedy().getRules().isEmpty()) {
					for (Rule remedy : prohibition.getRemedy().getRules()) {
						if (!(remedy instanceof Duty)) {
							addWarning("Invalid Prohibition: remedy must be of type Duty.",testedElement);//TODO covered in the Model
						}
						if (remedy instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequences()!=null) {
								addWarning("Invalid remedy duty: remedy-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} else if (testedElement instanceof Duty duty) {
				if (duty.getConsequences()!=null && duty.getConsequences().getRules()!=null && !duty.getConsequences().getRules().isEmpty()) {
					for (Rule consequence : duty.getConsequences().getRules()) {
						if (!(consequence instanceof Duty)) {
							addWarning("Invalid Duty: consequence must be of type Duty.",testedElement);//TODO covered in the Model
						}
						if (consequence instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequences()!=null) {
								addWarning("Invalid consequence duty: consequence-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} 
		}		
	}
	private void addWarning(String warning, ODRLClassImpl object) {//possibly add with list of ODRLClassImpl-objects
		host.appendLineToReport("Warning: " + warning + " Found in a " + object.getClass().getSimpleName() + " contained in the " + "object.containingUMLElement.getClass()" + " named " + "object.containingUMLElement.getName()");//TODO: unimplemented parts
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