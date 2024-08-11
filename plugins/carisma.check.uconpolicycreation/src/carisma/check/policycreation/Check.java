package carisma.check.policycreation;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.core.internal.resources.ResourceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
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
import carisma.check.policycreation.profileclasses.ODRLClass;
import carisma.check.policycreation.profileclasses.core.asset.AssetCollection;
import carisma.check.policycreation.profileclasses.core.constraints.Constraint;
import carisma.check.policycreation.profileclasses.core.party.PartyCollection;
import carisma.check.policycreation.profileclasses.core.policy.Agreement;
import carisma.check.policycreation.profileclasses.core.policy.Offer;
import carisma.check.policycreation.profileclasses.core.policy.Policy;
import carisma.check.policycreation.profileclasses.core.relation.Relation;
import carisma.check.policycreation.profileclasses.core.relation.Target;
import carisma.check.policycreation.profileclasses.core.rule.Duty;
import carisma.check.policycreation.profileclasses.core.rule.Permission;
import carisma.check.policycreation.profileclasses.core.rule.Prohibition;
import carisma.check.policycreation.profileclasses.core.rule.Rule;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";

	public static final String CHECK_NAME = "Policy Model Transformation";
	
//--------------
	AnalysisHost host;

	Map<EObject,ODRLClass> referencingList2 = new HashMap<>();
	Map<String,Collection<ODRLClass>> typeBuckets = new HashMap<String,Collection<ODRLClass>>();//TODO Not used anymore
	ODRLClass root;
	String policyString;
	Package usedPackage;
	//Map<JSONObject>
	final static String typeString = "@type";
	final static String nullString = "Null";
	//possibly add emptyString-Variable since empty strings are treated as Null-Strings
	final static String profileName = "ODRLCommonVocabulary";
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
		List<ODRLClass> objectList = new LinkedList<ODRLClass>();
		Collection<Element> modelContents = inputModel.allOwnedElements();
		UMLModelConverter converter = new UMLModelConverter();
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
					System.out.println("Structural features of a stereotype application:");
					for(EStructuralFeature stAppEsf : e.getStereotypeApplication(s).eClass().getEAllStructuralFeatures()) {
						System.out.println(stAppEsf);
					}
					System.out.println("Features end");
					Object object = (addElement(e.getStereotypeApplication(s), null, e));
					
					if (object instanceof ODRLClass odrlC) {
						System.out.println("Created ODRLObject: " + odrlC);
						JSONObject jso = new JSONObject(odrlC);
						//					
						Object converterMap = converter.printMap(odrlC);
						System.out.println("converter print map");
						System.out.println(converterMap);
						System.out.println("converter print JSON");
						if (converterMap instanceof Map actualMap)
						System.out.println(new JSONObject(actualMap).toString(4));
						else System.out.println(converterMap==null?"Convertermap is null" : converterMap.getClass());
						System.out.println("converter printed to json");
						//
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

		for (ODRLClass odrlc : objectList) {
			if (odrlc instanceof Policy) {
				Object converterMap = converter.printMap(odrlc);
				if (converterMap instanceof Map actualMap) {
					String outString =(new JSONObject(actualMap).toString(4));
					policyString = outString;
					IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
					IPath p = Path.fromOSString("testp_/textfile.txt");//TODO change to parameter input
					IFile iFile = workspaceRoot.getFile(p);
					System.out.println("File Path: " + iFile.getLocation());
					InputStream in = new ByteArrayInputStream(outString.getBytes(StandardCharsets.UTF_8));
					try {
					iFile.create(in, false, null);
					in.close();
					}
					catch (ResourceException e) {
						//TODO: Don't print, add as Error to analysis results
						System.out.println(e.getMessage());
					}
					catch (Exception e) {
						e.printStackTrace();
					}
				}
				
			}
			checkProfileRequirements(odrlc);
		}
//		for (JSONObject jobj : printList) {
//			System.out.println(jobj.toString(4));
//			System.out.println();
//		}
	}
		
		
		
	
	
	
	///////////////////////////////
	private Object addElement(EObject currentEObject, ODRLClass odrlParent, Element activityElement) {//TODO modify the names if the eCore naming derives form the names in generated code
//		odrlSwitch.doSwitch(currentEObject);//TODO remove
//		System.out.println("Classifier id: " + currentEObject.eClass().getClassifierID());//TODO remove
		if (currentEObject==null) {//Not necessary for result
			return null;
		}
//		if (referencingList2.get(currentEObject)!= null) {//TODO either add parent to parents-Attribute here (and before the other return-statement) or in the method processing the return value (if that's where it's decided whether it's actually added (for example in case it would, but must not ovewrite a value of the parent))
//			System.out.println("already generated: " + currentEObject);
//			return referencingList2.get(currentEObject);//Problem: Same enums are used in the generation of different ODRL-Objects (e.g. one action-enum with several different refinements)
//		}
		String objectClassName = currentEObject.eClass().getName();
		Object newObject = null;
		
		UMLModelConverter converter = new UMLModelConverter();
		//Filling the generated object
		newObject = converter.addElement(currentEObject, odrlParent, activityElement);
		//fill(currentEObject, newObject, activityElement);//TODO maybe return boolean with the fill()-method to signal whether it was filled sufficiently
		
		
		//
//		if (newObject instanceof ODRLClassImpl newOdrlObject) {
//			System.out.println("At end of addElement :" + newObject);
//			referencingList2.put(currentEObject, newOdrlObject);//Maybe extend the valid keys and add all objects, not just ODRLClassImples
//		}
		System.out.println(converter.addElement(currentEObject,null,null)==null?"Helper: Null": "Helper:  " + converter.addElement(currentEObject,null,null)); //TODO: Watch out: 2nd call
		System.out.println("passed EObject: " + currentEObject);
		return newObject;
	}
	
	
	
	private <T> List<T> addElement(List currentList, ODRLClass odrlParent, Element activityElement, Class<T> type) {//No check for several layers of lists as that case does not occur in the current model
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
	
	private String addElement(String currentObject, ODRLClass odrlParent, EObject activityElement) {
		return currentObject;
	}
	
	
//	private Object getValue(EObject eObject, String featureName) {
////		EStructuralFeature feature = eObject.eClass().getEStructuralFeature(featureName);
////		if(feature == null) {
////			//TODO add missing feature information?
////		}
////		return feature==null ? null : eObject.eGet(feature);
//		return eObject.eGet(eObject.eClass().getEStructuralFeature(featureName));//Nullpointer-exception with null-feature can only be produced by code errors, not by input errors
//	}
	
	
	///////////////////////////////
	
	private void checkProfileRequirements(ODRLClass testedElement) {//replace testedElement with the created objects
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
							if (consequenceDuty.getConsequence()!=null) {
								addWarning("Invalid remedy duty: remedy-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} else if (testedElement instanceof Duty duty) {
				if (duty.getConsequence()!=null && duty.getConsequence().getRules()!=null && !duty.getConsequence().getRules().isEmpty()) {
					for (Rule consequence : duty.getConsequence().getRules()) {
						if (!(consequence instanceof Duty)) {
							addWarning("Invalid Duty: consequence must be of type Duty.",testedElement);//TODO covered in the Model
						}
						if (consequence instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequence()!=null) {
								addWarning("Invalid consequence duty: consequence-Duty must not have a consequence itself.",testedElement);
							}
						}
					}
				}
			} 
		}		
	}
	private void addWarning(String warning, ODRLClass object) {//possibly add with list of ODRLClassImpl-objects
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
	
	public String getPolicyString() {
		return policyString;
	}

	

}