package carisma.check.uconpolicycreation;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Activity;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Stereotype;
import org.json.JSONObject;

import ODRLCommonVocabulary.ODRLCommonVocabularyFactory;
import ODRLCommonVocabulary.ODRLCommonVocabularyPackage;
import carisma.check.uconpolicycreation.profileclasses.ODRLClass;
import carisma.check.uconpolicycreation.profileclasses.core.asset.AssetCollection;
import carisma.check.uconpolicycreation.profileclasses.core.constraints.Constraint;
import carisma.check.uconpolicycreation.profileclasses.core.function.Assigner;
import carisma.check.uconpolicycreation.profileclasses.core.function.Function;
import carisma.check.uconpolicycreation.profileclasses.core.party.Party;
import carisma.check.uconpolicycreation.profileclasses.core.party.PartyCollection;
import carisma.check.uconpolicycreation.profileclasses.core.policy.Agreement;
import carisma.check.uconpolicycreation.profileclasses.core.policy.Offer;
import carisma.check.uconpolicycreation.profileclasses.core.policy.Policy;
import carisma.check.uconpolicycreation.profileclasses.core.relation.Relation;
import carisma.check.uconpolicycreation.profileclasses.core.relation.Target;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Duty;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Permission;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Prohibition;
import carisma.check.uconpolicycreation.profileclasses.core.rule.Rule;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;

/** Contains a Simple CARiSMA Check which returns all elements of a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";
	public static final String PARAM_OUTPUTFILE = "carisma.check.uconpolicycreation.outputfile";
	public static final String PARAM_CREATEPOLICY = "carisma.check.uconpolicycreation.createoutputfile";
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
	UMLModelConverter modelConversionHandler;
	

	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		System.out.println("Starting Policycheck performance");
		this.host = host;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (currentModel.getContents().get(0) instanceof Package model) {

			try {
			modelConversionHandler = new UMLModelConverter("resources" + File.separator + "odrl_jsonld_context_with_added_id.txt");
			} catch (IOException e) {
				//TODO: Add Warning: no JSON-LD-context given
				return false;
			}
			//Collection<Element> modelContents = model.allOwnedElements();
			
			
			structureModel(model, modelConversionHandler);
			
			//Check policy-validation
			boolean policyValidChecked = true;
			for (ODRLClass policyElement : modelConversionHandler.getHandledOdrlObjects()) {
				boolean elementValid = checkProfileRequirements(policyElement);
				if (!elementValid) {
					policyValidChecked = false;
				}
			}
			if (!policyValidChecked) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "No policy-File was created. The resulting policy does not not adhere to the odrl-specification."));
				return false;
			}
			
			boolean createPolicyFile = parameters != null && ((BooleanParameter) parameters.get(PARAM_CREATEPOLICY)).getValue();
			if (createPolicyFile) {
				File file = ((OutputFileParameter) parameters.get(PARAM_OUTPUTFILE)).getValue();
				try (BufferedWriter writer = new BufferedWriter(new FileWriter(file, false))){
					writer.write(policyString);
				} catch (IOException e) {
					//host.appendLineToReport(e.getMessage());
					host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No policy-file was created. " +  e.getMessage()));
					return false;
				}
			}

			//printContent(model, "");
			
			//host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, "number of elements counted: "+numOfElements));
			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, (createPolicyFile ? ("Policy file \"" + ((OutputFileParameter) parameters.get(PARAM_OUTPUTFILE)).getValue() + "\" has been created. ") : "No policy-file has been created due to the createpolicyfile-parameter. " ) + "No errors were found in the policy."));
			return true;
			//------
		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	

	private void structureModel(Package inputModel, UMLModelConverter converter) {
		List<JSONObject> printList = new LinkedList<JSONObject>();
		List<ODRLClass> objectList = new LinkedList<ODRLClass>();
		Collection<Element> modelContents = inputModel.allOwnedElements();

		
		for (Element e : modelContents) {
			if (!(e instanceof Activity)) {
				continue;
			}		
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals(profileName)) { //probably replace qualified name comparison by an more unique identifier
					Object object = (converter.addElement(e.getStereotypeApplication(s), null, e));
					
//					if (object instanceof ODRLClass odrlC) {
//						System.out.println("Created ODRLObject: " + odrlC);
//						JSONObject jso = new JSONObject(odrlC);
//						//					
//						Object converterMap = converter.startMap(odrlC);
//						System.out.println("converter print map");
//						System.out.println(converterMap);
//						System.out.println("converter print JSON");
//						if (converterMap instanceof Map actualMap)
//						System.out.println(new JSONObject(actualMap).toString(4));
//						else System.out.println(converterMap==null?"Convertermap is null" : converterMap.getClass());
//						System.out.println("converter printed to json");
//						//
//						printList.add(jso);
//						objectList.add(odrlC);
//						System.out.println(jso.toString(4));
//					}
//					else {
//						System.out.println("Null: " + e.getStereotypeApplication(s));
//						for (EStructuralFeature esf : e.getStereotypeApplication(s).eClass().getEStructuralFeatures()) {
//							System.out.println("ESF: " + esf);
//						}
//					}
//					System.out.println();
				}			
			}
		}
		String string1;
		Object converterMap = converter.startMap(converter.getPolicyRoot());
		if (converterMap instanceof Map<?,?> actualMap) {
			policyString =(new JSONObject(actualMap).toString(4));
			//policyString = outString;
		}
	}
		
	//TODO change to more efficient approach (such as adding Elements to Class-Maps and accessing Classes and their Superclasses through their map-entries for the checks or adding the validity checks to the ODRLClasses themselves)
	private boolean checkProfileRequirements(ODRLClass testedElement) {//replace testedElement with the created objects
		boolean validPolicy = true;
		//TODO no checks for valid form of IRIs so far
		if (testedElement instanceof Policy policy) {
			if (policy.getPermission().isEmpty()
					&& policy.getProhibition().isEmpty()
					&& policy.getObligation().isEmpty()) {
				addWarning("Invalid Policy: Policy needs to have at least one permission, prohibition or obligation.",testedElement);
				validPolicy = false;
			}
			
			if (policy instanceof Offer offer) {
				//TODO one assigner (only one? needed with every rule or just with one?)
				//TODO Propably in separate method
//				Assigner assigner = null;
//				for (Permission rule : offer.getPermission()) {
//					List<Function> functions = rule.getInvolvedParties();
//					if (functions.size()==1) {
//						if (functions.get(0) instanceof Assigner newAssigner) {
//							if (assigner == null) {
//								assigner = newAssigner;
//							} else if (!assigner.getParty().equals(newAssigner.getParty())) {
//								addWarning("Invalid Offer: Rules of an offer must not contain multiple distinct Assigners.",testedElement);
//								validPolicy = false;
//							}
//						}
//						else {
//							addWarning("Invalid Offer: Rules of an offer must not contain Parties in Roles other than Assigners.",testedElement);
//						}
//					} else {
//						addWarning("Invalid Offer: Rules of an offer must not contain Parties in multiple roles.",testedElement);
//					}
//				}
//				for (Prohibition rule : offer.getProhibition()) {
//					List<Function> functions = rule.getInvolvedParties();
//					if (functions.size()==1) {
//						if (functions.get(0) instanceof Assigner newAssigner) {
//							if (assigner == null) {
//								assigner = newAssigner;
//							} else if (!assigner.getParty().equals(newAssigner.getParty())) {
//								addWarning("Invalid Offer: Rules of an offer must not contain multiple distinct Assigners.",testedElement);
//								validPolicy = false;
//							}
//						}
//						else {
//							addWarning("Invalid Offer: Rules of an offer must not contain Parties in Roles other than Assigners.",testedElement);
//						}
//					} else {
//						addWarning("Invalid Offer: Rules of an offer must not contain Parties in multiple roles.",testedElement);
//					}
//				}
//				for (Duty rule : offer.getObligation()) {
//					List<Function> functions = rule.getInvolvedParties();
//					if (functions.size()==1) {
//						if (functions.get(0) instanceof Assigner newAssigner) {
//							if (assigner == null) {
//								assigner = newAssigner;
//							} else if (!assigner.getParty().equals(newAssigner.getParty())) {
//								addWarning("Invalid Offer: Rules of an offer must not contain multiple distinct Assigners.",testedElement);
//								validPolicy = false;
//							}
//						}
//						else {
//							addWarning("Invalid Offer: Rules of an offer must not contain Parties in Roles other than Assigners.",testedElement);
//						}
//					} else {
//						addWarning("Invalid Offer: Rules of an offer must not contain Parties in multiple roles.",testedElement);
//					}
//				}
				
			} else if (policy instanceof Agreement agreement) {
				//TODO one assigner, one assignee
			}
		} else if (testedElement instanceof AssetCollection assetCollection) {
			if (assetCollection.getRefinement()!=null
					&& (assetCollection.getSource()==null || assetCollection.getSource().isEmpty())) {
				addWarning("Invalid AssetCollection: Source-property needs to be used with refinement.", testedElement);
				validPolicy = false;
			}
		} else if (testedElement instanceof PartyCollection partyCollection) {
			if (partyCollection.getRefinement()!=null
					&& (partyCollection.getSource()==null  || partyCollection.getSource().isEmpty())) {
				addWarning("Invalid PartyCollection: Source-property needs to be used with refinement.", testedElement);
				validPolicy = false;
			}
		} else if (testedElement instanceof Constraint constraint) {
			if (constraint.getLeftOperand()==null) {
				addWarning("Invalid Constraint: Needs to have a leftOperand selected.",testedElement);
				validPolicy = false;
			}
			if (constraint.getOperator()==null) {
				addWarning("Invalid Constraint: Needs to have an operator selected.",testedElement);
				validPolicy = false;
			}
			if ( (constraint.getRightOperand()==null
					||constraint.getRightOperand().isEmpty() 
					) && ( constraint.getRightOperandReference()==null
					||constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: Needs to have either a rightOperand, or rightOperandReference, has neither.",testedElement);
				validPolicy = false;
			}
			if ( (constraint.getRightOperand()!=null
					&& !constraint.getRightOperand().isEmpty() 
					 && constraint.getRightOperandReference()!=null
					&& !constraint.getRightOperandReference().isEmpty())) {
				addWarning("Invalid Constraint: Must not have both rightOperand and rightOperandReference.",testedElement);//TODO check if restriction actually exists
				validPolicy = false;
			}
		}
		if (testedElement instanceof Rule rule) {
			if (rule.getAction() == null) {
				addWarning("Invalid Rule: Needs to have an action selected.",testedElement);
				validPolicy = false;
			}
			if (testedElement instanceof Permission permission) {
				boolean hasTarget = false;
				for (Relation relation : permission.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid Permission: Needs to have a relation of type target.",testedElement);
					validPolicy = false;
				}
			} else if (testedElement instanceof Prohibition prohibition) {
				boolean hasTarget = false;
				for (Relation relation : prohibition.getInvolvedAssets()) {
					if (relation instanceof Target) {
						hasTarget = true;
					}
				}
				if (!hasTarget) {
					addWarning("Invalid Prohibition: Needs to have a relation of type target.",testedElement);
					validPolicy = false;
				}
				if (prohibition.getRemedy()!=null && prohibition.getRemedy().getRules()!=null && !prohibition.getRemedy().getRules().isEmpty()) {
					for (Rule remedy : prohibition.getRemedy().getRules()) {
						if (!(remedy instanceof Duty)) {
							addWarning("Invalid Prohibition: Remedy must be of type Duty.",testedElement);//covered in the Model
							validPolicy = false;
						}
						if (remedy instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequence()!=null) {
								addWarning("Invalid Remedy Duty: Remedy-Duty must not have a consequence itself.",testedElement);
								validPolicy = false;
							}
						}
					}
				}
			} else if (testedElement instanceof Duty duty) {
				if (duty.getConsequence()!=null && duty.getConsequence().getRules()!=null && !duty.getConsequence().getRules().isEmpty()) {
					for (Rule consequence : duty.getConsequence().getRules()) {
						if (!(consequence instanceof Duty)) {
							addWarning("Invalid Duty: Consequence must be of type Duty.",testedElement);//covered in the Model
							validPolicy = false;
						}
						if (consequence instanceof Duty consequenceDuty) {
							if (consequenceDuty.getConsequence()!=null) {
								addWarning("Invalid Consequence Duty: Consequence-Duty must not have a consequence itself.",testedElement);//possibly instead refer to the consequence-duty
								validPolicy = false;
							}
						}
					}
				}
			} 
		}
		return validPolicy;
	}
	
	
	private void addWarning(String warning, ODRLClass object) {//possibly add with list of ODRLClassImpl-objects
		Element baseElement = object.gatContainingUmlElement();

		String string = "Warning: " + warning;
		if (baseElement != null) {
			string += " Found in a " + object.getClass().getSimpleName();
			//TODO the baseElement
			if (baseElement instanceof NamedElement named) {
				string +=  " contained in the " + baseElement.getClass().getSimpleName() + " named " + named.getName() + ".";
			} else {
				string += " contained in a " + baseElement.getClass().getSimpleName() + ".";
			}
		}
		
		host.appendLineToReport(string);//TODO: possibly add diagram name representation and directly containing ODRLClass to classes to be referred here
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