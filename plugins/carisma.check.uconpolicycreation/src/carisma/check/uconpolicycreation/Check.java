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

/** Contains a CARiSMA Check which creates a machine-readable Usage Control Policy from a given Model.
 *
 */

public class Check implements CarismaCheckWithID {
	public static final String CHECK_ID = "carisma.check.policycreation";
	public static final String PARAM_OUTPUTFILE = "carisma.check.uconpolicycreation.outputfile";
	public static final String PARAM_CREATEPOLICY = "carisma.check.uconpolicycreation.createoutputfile";
	public static final String CHECK_NAME = "Policy Model Transformation";
	
	AnalysisHost host;
	/**
	 * Contains the Policy created in this Object (in JSON-LD-Form).
	 */
	String policyString;

	static final String PROFILE_NAME = "ODRLCommonVocabulary";
	/**
	 * Package generated from the used profile.
	 */
	ODRLCommonVocabularyPackage odrlPackage = ODRLCommonVocabularyPackage.eINSTANCE;
	/**
	 * Contains the Object handling model conversion for this class.
	 */
	UMLModelConverter modelConversionHandler;
	

	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
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
				host.appendLineToReport(e.toString());
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "Context file could not be loaded."));
				return false;
			}
			
			//Create policy
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
				//Create file containing policy
				File file = ((OutputFileParameter) parameters.get(PARAM_OUTPUTFILE)).getValue();
				try (BufferedWriter writer = new BufferedWriter(new FileWriter(file, false))){
					writer.write(policyString);
				} catch (IOException e) {
					host.appendLineToReport(e.toString());
					host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No policy-file was created. "));
					return false;
				}
			}

			host.addResultMessage(new AnalysisResultMessage(StatusType.INFO, (createPolicyFile ? ("Policy file \"" + ((OutputFileParameter) parameters.get(PARAM_OUTPUTFILE)).getValue() + "\" has been created. ") : "No policy-file has been created due to the createpolicyfile-parameter. " ) + "No errors were found in the policy."));
			return true;

		}
		host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
		return false;
	}
	
	/**
	 * Creates a JSON-LD-Text of the Policy contained in a model using an {@link UMLModelConverter} and saves it in this {@link Check}'s {@link #policyString}-Attribute.
	 * 
	 * @param inputModel {@link Package} converted to a policy
	 * @param converter {@link UMLModelConverter} handling the conversions
	 */
	private void structureModel(Package inputModel, UMLModelConverter converter) {
		Collection<Element> modelContents = inputModel.allOwnedElements();	
		for (Element e : modelContents) {//only passes stereotypes on activities to the converter
			if (!(e instanceof Activity)) {
				continue;
			}		
			for (Stereotype s : e.getAppliedStereotypes()) {
				if (s.getProfile().getQualifiedName().equals(PROFILE_NAME)) { //probably replace qualified name comparison by an more unique identifier
					converter.addElement(e.getStereotypeApplication(s), null, e);
				}			
			}
		}
		Object converterMap = converter.startMap();
		if (converterMap instanceof Map<?,?> actualMap) {
			policyString =(new JSONObject(actualMap).toString(4));
		}
	}
		
	//TODO change to more efficient approach (such as adding Elements to Class-Maps and accessing Classes and their Superclasses through their map-entries for the checks or adding the validity checks directly to the ODRLClasses themselves)
	/**
	 * Checks whether an {@link ODRLClass} adheres to the odrl-specification.
	 * 
	 * @param testedElement {@link ODRLClass} to be tested
	 * @return whether the Element does adhere
	 */
	private boolean checkProfileRequirements(ODRLClass testedElement) {
		// no checks for valid form of IRIs so far
		boolean validPolicy = true;
		if (testedElement instanceof Policy policy) {
			if (policy.getPermission().isEmpty()
					&& policy.getProhibition().isEmpty()
					&& policy.getObligation().isEmpty()) {
				addWarning("Invalid Policy: Policy needs to have at least one permission, prohibition or obligation.",testedElement);
				validPolicy = false;
			}
			
			if (policy instanceof Offer offer) {
				//one assigner (only one? needed with every rule or just with one?)				
			} else if (policy instanceof Agreement agreement) {
				// one assigner, one assignee
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
	
	/**
	 * Adds a warning message to the analysis report of this {@link Check}.
	 * 
	 * @param warning part of the message that contains the warning itself
	 * @param object object for which the warning is created
	 */
	private void addWarning(String warning, ODRLClass object) {//possibly add with list of ODRLClassImpl-objects
		Element baseElement = object.gatContainingUmlElement();

		String string = "Warning: " + warning;
		if (baseElement != null) {
			string += " Found in a " + object.getClass().getSimpleName();
			//baseElement takes  Impl-Classes so not entirely accurate for the diagram
			if (baseElement instanceof NamedElement named) {
				string +=  " contained in the " + baseElement.getClass().getSimpleName() + " named " + named.getName() + ".";
			} else {
				string += " contained in a " + baseElement.getClass().getSimpleName() + ".";
			}
		}
		
		host.appendLineToReport(string);// possibly add diagram name representation and directly containing ODRLClass to classes to be referred here
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