/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.check.smartcard.evolution.authorizedstatuscheck;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Namespace;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.authorizedstatus.AuthorizedStatusCheck;
import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.check.smartcard.utils.OutputTarget;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheck;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.EObjectUtil;
import carisma.evolution.AddElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.ModifierMap;
import carisma.evolution.uml2.UMLModifier;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.UMLStateMachineHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public class AuthorizedStatusEvolutionModifierCheck implements CarismaCheck {

	public static final String DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";
	public static final String MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";

	private AnalysisHost host;
	
	private DeltaList deltaList = null;
	private ModifierMap deltaModifiers = null;
	
	private UMLModifier deltaModifier = null;
	
	private AuthorizedStatusCheck authorizedChecks = null;
	
	private List<AnalysisMessage> errorMessages = null;
	
	private List<DeltaElement> processedDeltaElements = null;
	private Map<Transition, DeltaElement> processedTransitions = null;
	
	/**
	 * Constant String for the name of the TaggedValue 'permission'.
	 */
	private static final String PERMISSION = "permission";
	
	public AuthorizedStatusEvolutionModifierCheck() {
		processedDeltaElements = new ArrayList<DeltaElement>();
		processedTransitions = new HashMap<Transition, DeltaElement>();
		errorMessages = new ArrayList<AnalysisMessage>();		
	}
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
		host = newHost;
		Resource currentModel = host.getAnalyzedModel();
		if (currentModel.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModel.getContents().get(0) instanceof Model)) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		
		try {
			deltaList = (DeltaList) host.getFromRegister(DELTAS_REGISTER_KEY);
			deltaModifiers = (ModifierMap) host.getFromRegister(MODIFIERS_REGISTRY_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		if (deltaList == null || deltaModifiers == null) {
			return false;
		}
		if (deltaList.isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No deltas left to analyze."));
			return true;
		}
		int beforeMaxChanges = deltaList.getHighestChangeCountNow();
		boolean isSuccessful = checkDeltas();
		if (isSuccessful) {
			host.addResultMessage(
					new AnalysisResultMessage(
							StatusType.INFO,
							"A successful maximum Delta (using " + deltaList.getHighestChangeCountNow() + " changes) exists."));
		} else {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (max: " + beforeMaxChanges + " changes) found."));
			if (deltaList.getHighestChangeCountNow() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<authorized-status>>."));
			} else {
				host.addResultMessage(
						new AnalysisResultMessage(
								StatusType.ERROR,
								"Maximum successful Delta has " + deltaList.getHighestChangeCountNow() + " changes."));
			}
		}
		return isSuccessful;
	}
	
	private boolean checkDeltas() {
		boolean hasMaxSuccessfulDelta = false;
		int deltaCounter = 1;
		List<Delta> violatingEvolutions = new ArrayList<Delta>();
		for (Delta d : deltaList.getRemainingDeltas()) {
			boolean deltaSuccessful = true;
			checkDelta(d);
			for (AnalysisMessage errorMessage : errorMessages) {
				if (errorMessage.getType() == StatusType.ERROR) {
					violatingEvolutions.add(d);
					deltaSuccessful = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : errorMessages) {
				errorMessage.print(host, "Delta " + deltaCounter + ":");
			}
			if (deltaSuccessful
			        && d.getNumberOfUsedChanges() == deltaList.getHighestChangeCountNow()) {
				hasMaxSuccessfulDelta = true;
			}
			deltaCounter++;
		}
		deltaList.removeAll(violatingEvolutions);
		return hasMaxSuccessfulDelta;
	}
	
	private void checkDelta(final Delta d) {
		init(d);
		processDelElements(d.getAllDeletions());
		processAddElements(d.getAllAdditions());
		processSubstElements(d.getAllSubstitutions());	
		processEditElements(d.getAllEdits());
		processCopyElements(d.getAllCopies());
	}
		
	private void init(final Delta d) {
		deltaModifier = deltaModifiers.get(d);
		if (errorMessages == null) {
			errorMessages = new ArrayList<AnalysisMessage>();
		}
		errorMessages.clear();
		if (processedDeltaElements == null) {
			processedDeltaElements = new ArrayList<DeltaElement>();
		}
		processedDeltaElements.clear();
		if (processedTransitions == null) {
			processedTransitions = new HashMap<Transition, DeltaElement>();
		}
		processedTransitions.clear();
		authorizedChecks = new AuthorizedStatusCheck();
	}
	
	private void processAddElements(final List<AddElement> allAdditions) {
		for (AddElement addElement : allAdditions) {
			processAddElement(addElement);
			processedDeltaElements.add(addElement);
			processContent(addElement);
		}
	}
	
	private void processCopyElements(final List<CopyElement> allCopies) {
		for (CopyElement copyElement : allCopies) {
			processCopyElement(copyElement);
			processedDeltaElements.add(copyElement);
		}
	}
	
	private void processDelElements(final List<DelElement> allDeletions) {
		for (DelElement delElement : allDeletions) {
			processDelElement(delElement);
			processedDeltaElements.add(delElement);
		}
	}
	
	private void processEditElements(final List<EditElement> allEdits) {
		for (EditElement editElement : allEdits) {
			processEditElement(editElement);
			processedDeltaElements.add(editElement);
		}
	}
	
	private void processSubstElements(final List<SubstElement> allSubstitutions) {
		for (SubstElement substElement : allSubstitutions) {
			EObject target = substElement.getTarget();
			if (target instanceof Constraint) {
				processGuardSubstitution(substElement);
			} else {
				processDelElement(substElement);
				for (AddElement component : substElement.getComponents()) {
					processAddElement(component);
				}
			}
			processedDeltaElements.add(substElement);
		}
	}
	
	private void processContent(final AddElement addElement) {
		for (AddElement containedElement : addElement.getContent()) {
			processAddElement(containedElement);
			processedDeltaElements.add(containedElement);
			processContent(containedElement);
		}
	}
	
	private void processAddElement(final AddElement addElement) {
		EClass newElemClass = addElement.getMetaClass();
		if (newElemClass.getName().equalsIgnoreCase("Transition")) {
			processTransitionAddition(addElement);
		} else if (newElemClass.getName().equalsIgnoreCase("Property") 
				&& addElement.getTarget() instanceof StereotypeApplication 
				&& ((StereotypeApplication) addElement.getTarget()).getAppliedStereotype().getName().equalsIgnoreCase("authorized-status")) {
			processPermissionAddition(addElement);
		}
	}
	
	private void processCopyElement(final CopyElement copyElement) {
		EObject target = copyElement.getTarget();
		if (target instanceof Transition) {
			processTransitionCopy(copyElement);
		} else if (target instanceof TaggedValue) {
			TaggedValue copiedTag = (TaggedValue) target;
			if (copiedTag.getName().equalsIgnoreCase(PERMISSION) && copiedTag.getElement() instanceof State) {
				processPermissionCopy(copyElement);
			}
		} else if (target instanceof Constraint && target.eContainer() instanceof Transition) {
			processGuardCopy(copyElement);
		}
	}
	
	private void processDelElement(final DeltaElement delElement) {
		EObject affectedElem = delElement.getTarget();
		if (affectedElem instanceof Constraint) {
			processGuardDeletion((DelElement) delElement);
		}	
	}
	
	//FIXME: edit permission, edit transition, edit owner, edit guard
	private void processEditElement(final EditElement editElement) {
		EObject target = editElement.getTarget();
		if (target instanceof Transition) {
			processTransitionEdit(editElement);
		} else if (target instanceof Constraint) {
			processConstraintEdit(editElement);
		} else if (target instanceof TaggedValue) {
			TaggedValue editedTag = (TaggedValue) target;
			if (editedTag.getName().equalsIgnoreCase(PERMISSION) && editedTag.getElement() instanceof State) {
				processPermissionEdit(editElement);
			}
		}
	}
	
	private void processTransitionCopy(final CopyElement copyElement) {
		Transition oldCopiedTransition = (Transition) copyElement.getTarget();
		CopyElement copyCopy = (CopyElement) deltaModifier.get(copyElement);
		String newName = (String) copyCopy.getChangedValues().get("name");
		if (newName == null) {
			newName = oldCopiedTransition.getName();
		}
		Transition newTransition = null;
		try {
			newTransition = UMLHelper.getElementOfNameAndType(deltaModifier.getModifiedModel(), newName, Transition.class);
	        State targetState = (State) newTransition.getTarget();
	        StereotypeApplication authApp = UMLsecUtil.getStereotypeApplication(targetState, UMLsec.AUTHORIZED_STATUS);
	        if (authApp != null) {
	            String permission = (String) authApp.getTaggedValue(PERMISSION).getValue();
	            if (!(authorizedChecks.checkAuthorizedState(targetState, permission).isEmpty())) {
	                errorMessages.add(new AnalysisMessage(StatusType.ERROR, 
	                        OutputTarget.BOTH, Messages.copiedTransitionNotAdjusted(newTransition, targetState, permission)));
	            }
	        }
		} catch (ModelElementNotFoundException e) {
			errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
			        "Couldn't find copy of transition " + EObjectUtil.getTypeAndName(oldCopiedTransition) + ". Message: " + e.getMessage()));
		}
	}
	
	private void processPermissionCopy(final CopyElement copyElement) {
		TaggedValue oldCopiedPermission = (TaggedValue) copyElement.getTarget();
		String copiedPermission = (String) oldCopiedPermission.getValue();
		CopyElement copyCopy = (CopyElement) deltaModifier.get(copyElement);
		State receivingState = (State) copyCopy.getReceivingElement();
		for (Transition incomingTransition : receivingState.getIncomings()) {
			if (!authorizedChecks.checkIncomingTransition(incomingTransition, copiedPermission).isEmpty()) {
				errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
				        Messages.copiedPermissionGuardNotAdjusted(receivingState, copiedPermission, incomingTransition)));
			}
		}
	}
	
	private void processGuardCopy(final CopyElement copyElement) {
		CopyElement copyCopy = (CopyElement) deltaModifier.get(copyElement);
		Transition receivingTransition = (Transition) copyCopy.getReceivingElement();
		if (!authorizedChecks.checkIncomingTransition(receivingTransition).isEmpty()) {
			errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.copiedGuardIncorrect(receivingTransition)));
		}
	}
	
	private void processTransitionEdit(final EditElement editElement) {
		EditElement editCopy = (EditElement) deltaModifier.get(editElement);
		Transition editedTransition = (Transition) editCopy.getTarget();
		if (!authorizedChecks.checkIncomingTransition(editedTransition).isEmpty()) {
			errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.editedTransitionWrongGuard(editedTransition)));
		}
	}
	
	private void processConstraintEdit(final EditElement editElement) {
		EditElement editCopy = (EditElement) deltaModifier.get(editElement);
		Constraint editedConstraint = (Constraint) editCopy.getTarget();
		Transition editedTransition = (Transition) editedConstraint.getOwner();
		if (!authorizedChecks.checkIncomingTransition(editedTransition).isEmpty()) {
			errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.editedGuardWrong(editedTransition)));
		}
	}
	
	private void processPermissionEdit(final EditElement editElement) {
		EditElement editCopy = (EditElement) deltaModifier.get(editElement);
		TaggedValue editedTagValue = (TaggedValue) editCopy.getTarget();
		State authState = (State) editedTagValue.getElement();
		for (Transition incomingTransition : authState.getIncomings()) {
			if (!authorizedChecks.checkIncomingTransition(incomingTransition).isEmpty()) {
				errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
				        Messages.editedPermissionTransitionNotAdjusted(incomingTransition))); 
			}
		}
	}
	
	private void processGuardDeletion(final DelElement delElement) {
		Constraint constraint = (Constraint) delElement.getTarget();
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(constraint);
		Transition owningTransition = UMLStateMachineHelper.getTransition(guard);
		if (!processedTransitions.containsKey(owningTransition)) {
			processedTransitions.put(owningTransition, delElement);
			Transition owningTransitionCopy = (Transition) deltaModifier.getMapping().get(owningTransition);			
			State targetState = UMLStateMachineHelper.getTargetState(guard);
			State targetStateCopy = (State) deltaModifier.getMapping().get(targetState);
			String permissionStringCopy = AuthorizedStatusCheck.getPermission(targetStateCopy);
			if (owningTransitionCopy != null 
			        && targetStateCopy != null 
			        && permissionStringCopy != null
			        && !authorizedChecks.checkIncomingTransition(owningTransitionCopy, permissionStringCopy).isEmpty()) {
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.deletedNecessaryGuard(owningTransitionCopy, guard, permissionStringCopy)));
			}
		}
	}
	
	private void processTransitionAddition(final AddElement addElement) {
		AddElement addCopy = (AddElement) deltaModifier.get(addElement);
		Namespace newTransitionContainer = (Namespace) addCopy.getTarget();
		Transition newTransition = (Transition) newTransitionContainer.getMember((String) addCopy.getValues().get("name"));
		if (!processedTransitions.containsKey(newTransition)) {
			processedTransitions.put(newTransition, addElement);
			State targetState = (State) newTransition.getTarget();
			String sourceState = newTransition.getSource().getName();
			String permission = AuthorizedStatusCheck.getPermission(targetState);
			if (targetState != null 
			        && permission != null
			        && !authorizedChecks.checkIncomingTransition(newTransition, permission).isEmpty()) {
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedTransitionWrongGuard(targetState, sourceState, permission, AuthorizedStatusCheck.getGuardString(newTransition))));
			}
		}
	}
	
	private void processPermissionAddition(final AddElement addElement) {
		AddElement addCopy = (AddElement) deltaModifier.get(addElement);		
		StereotypeApplication stapp = (StereotypeApplication) addCopy.getTarget();
		State changedState = (State) stapp.getExtendedElement();
		if (changedState != null) {
    		String permission = (String) changedState.getValue(stapp.getAppliedStereotype(), PERMISSION);
    		if (permission != null) {
    			for (Transition incomingTransition : changedState.getIncomings()) {
    				if (!processedTransitions.containsKey(incomingTransition)) {
    					processedTransitions.put(incomingTransition, addElement);
    					if (!authorizedChecks.checkIncomingTransition(incomingTransition, permission).isEmpty()) {
    						errorMessages.add(new AnalysisMessage(
    								StatusType.ERROR,
    								OutputTarget.BOTH,
    								Messages.newPermissionWrongGuard(incomingTransition, 
    								        changedState, permission, AuthorizedStatusCheck.getGuardString(incomingTransition))));						
    					}
    				}
    			}
    		}
		}
	}
	
	private void processGuardSubstitution(final SubstElement substElement) {
		Constraint constraint = (Constraint) substElement.getTarget();
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(constraint);
		Transition incomingTransition = UMLStateMachineHelper.getTransition(guard);
		Transition incomingTransitionCopy = (Transition) deltaModifier.getMapping().get(incomingTransition);
		State authorizedState = UMLStateMachineHelper.getTargetState(incomingTransitionCopy);
		String permission = AuthorizedStatusCheck.getPermission(authorizedState);
		if (incomingTransitionCopy != null 
		        && authorizedState != null 
		        && permission != null
		        && !processedTransitions.containsKey(incomingTransition)) {
			processedTransitions.put(incomingTransition, substElement);
			if (!authorizedChecks.checkIncomingTransition(incomingTransitionCopy, permission).isEmpty()) {				
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.substitutedGuardNotAppropriate(incomingTransition, authorizedState, guard, permission)));
			}
		}
		for (AddElement component : substElement.getComponents()) {
			processAddElement(component);
		}
	}
}
