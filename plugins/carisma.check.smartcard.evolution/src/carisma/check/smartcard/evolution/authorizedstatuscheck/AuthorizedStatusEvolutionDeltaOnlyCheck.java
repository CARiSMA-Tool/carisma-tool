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
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.authorizedstatus.AuthorizedStatus;
import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.check.smartcard.utils.OutputTarget;
import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.analysis.result.StatusType;
import carisma.core.checks.CarismaCheckWithID;
import carisma.core.checks.CheckParameter;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.AdditiveElement;
import carisma.evolution.CopyElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.UMLStateMachineHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public class AuthorizedStatusEvolutionDeltaOnlyCheck implements CarismaCheckWithID {

	private static final String CHECK_ID = "carisma.check.smartcard.evolution.authorizedstatusdeltaonlycheck";

	public static final String PRECONDITION_DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";

	private static final String CHECK_NAME = "Evolution-aware Authorized Status Check (Delta only version)";

	private AnalysisHost host;
	
	private DeltaList deltaList = null;

	private Model currentModel = null;
	
	private Delta activeDelta = null;	
	
	private List<AnalysisMessage> errorMessages = null;
	
	private List<DeltaElement> processedDeltaElements = null;
	
	private Map<Transition, DeltaElement> processedTransitions = null;
	
	private Map<State, DeltaElement> processedStates = null;
	
	private Map<Transition, State> processedIncomings = null;
	/**
	 * Constant String for the name of the TaggedValue 'permission'.
	 */
	private static final String PERMISSION = "permission";

	public AuthorizedStatusEvolutionDeltaOnlyCheck() {
		this.processedDeltaElements = new ArrayList<>();
		this.processedTransitions = new HashMap<>();
		this.processedStates = new HashMap<>();
		this.processedIncomings = new HashMap<>();
		this.errorMessages = new ArrayList<>();		
	}
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
		this.host = newHost;
		Resource currentModelRes = this.host.getAnalyzedModel();
		if (currentModelRes.getContents().isEmpty()) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModelRes.getContents().get(0) instanceof Model)) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		this.currentModel = (Model) currentModelRes.getContents().get(0);
		
		try {
			this.deltaList = (DeltaList) this.host.getFromRegister(PRECONDITION_DELTAS_REGISTER_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		if (this.deltaList == null) {
			return false;
		}
		if (this.deltaList.isEmpty()) {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No deltas left to analyze."));
			return true;
		}
		int beforeMaxChanges = this.deltaList.getHighestChangeCountNow();
		boolean isSuccessful = checkDeltas();
		if (isSuccessful) {
			this.host.addResultMessage(
					new AnalysisResultMessage(
							StatusType.INFO,
							"A successful maximum Delta (using " + this.deltaList.getHighestChangeCountNow() + " changes) exists."));
		} else {
			this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (max: " + beforeMaxChanges + " changes) found."));
			if (this.deltaList.getHighestChangeCountNow() == 0) {
				this.host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<authorized-status>>."));
			} else {
				this.host.addResultMessage(
						new AnalysisResultMessage(
								StatusType.ERROR,
								"Maximum successful Delta has " + this.deltaList.getHighestChangeCountNow() + " changes."));
			}
		}
		return isSuccessful;
	}
	
	private boolean checkDeltas() {
		boolean hasMaxSuccessfulDelta = false;
		int deltaCounter = 1;
		List<Delta> violatingEvolutions = new ArrayList<>();
		for (Delta d : this.deltaList.getRemainingDeltas()) {
			this.activeDelta = d;
			boolean deltaSuccessful = true;
			checkActiveDelta();
			for (AnalysisMessage errorMessage : this.errorMessages) {
				if (errorMessage.getType() == StatusType.ERROR) {
					violatingEvolutions.add(d);
					deltaSuccessful = false;
					break;
				}
			}
			for (AnalysisMessage errorMessage : this.errorMessages) {
				errorMessage.print(this.host, "Delta " + deltaCounter + ":");
			}
			if (deltaSuccessful
			        && d.getNumberOfUsedChanges() == this.deltaList.getHighestChangeCountNow()) {
				hasMaxSuccessfulDelta = true;
			}
			deltaCounter++;
		}
		this.deltaList.removeAll(violatingEvolutions);
		return hasMaxSuccessfulDelta;
	}
	
	private void checkActiveDelta() {
		init(this.activeDelta);
		processDelElements(this.activeDelta.getAllDeletions());
		processAddElements(this.activeDelta.getAllAdditions());
		processSubstElements(this.activeDelta.getAllSubstitutions());	
		processEditElements(this.activeDelta.getAllEdits());
		processCopyElements(this.activeDelta.getAllCopies());
	}
		
	private void init(final Delta d) {
		if (this.errorMessages == null) {
			this.errorMessages = new ArrayList<>();
		}
		this.errorMessages.clear();
		if (this.processedDeltaElements == null) {
			this.processedDeltaElements = new ArrayList<>();
		}
		this.processedDeltaElements.clear();
		if (this.processedTransitions == null) {
			this.processedTransitions = new HashMap<>();
		}
		this.processedTransitions.clear();
		if (this.processedStates == null) {
			this.processedStates = new HashMap<>();
		}
		this.processedTransitions.clear();
		if (this.processedIncomings == null) {
			this.processedIncomings = new HashMap<>();
		}
		this.processedIncomings.clear();
	}
	
	private void processAddElements(final List<AddElement> allAdditions) {
		for (AddElement addElement : allAdditions) {
			processAddElement(addElement);
			this.processedDeltaElements.add(addElement);
			processContent(addElement);
		}
	}
	
	private void processCopyElements(final List<CopyElement> allCopies) {
		for (CopyElement copyElement : allCopies) {
			processCopyElement(copyElement);
			this.processedDeltaElements.add(copyElement);
		}
	}
	
	private void processDelElements(final List<DelElement> allDeletions) {
		for (DelElement delElement : allDeletions) {
			processDelElement(delElement);
			this.processedDeltaElements.add(delElement);
		}
	}
	
	private void processEditElements(final List<EditElement> allEdits) {
		for (EditElement editElement : allEdits) {
			processEditElement(editElement);
			this.processedDeltaElements.add(editElement);
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
			this.processedDeltaElements.add(substElement);
		}
	}
	
	private void processContent(final AddElement addElement) {
		for (AddElement containedElement : addElement.getContent()) {
			processAddElement(containedElement);
			this.processedDeltaElements.add(containedElement);
			processContent(containedElement);
		}
	}
	
	private void processAddElement(final AddElement addElement) {
		EClass newElemClass = addElement.getMetaClass();
		Element changedModelElement = getChangedModelElement(addElement);
		if (newElemClass.getName().equalsIgnoreCase("Transition")) {
			processTransitionAddition(addElement);
		} else if (changedModelElement instanceof State) {
			State changedState = (State) changedModelElement;
			String newPermission = getAfterPermission(changedState);
			for (Transition t : changedState.getIncomings()) {
				String newGuard = getAfterGuard(t);
				if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
					this.errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.newPermissionWrongGuard(t, 
									changedState, newPermission, newGuard)));					
				}
			}
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
		Transition oldTransition = (Transition) copyElement.getTarget();
		String newTargetName = getElementName(copyElement.getChangedValues(), "target");
		State newTargetState = null;
		try {
			newTargetState = UMLHelper.getElementOfNameAndType(this.currentModel, newTargetName, State.class);
		} catch (ModelElementNotFoundException e) {
			// TODO: catch
		}
		String newPermission = getAfterPermission(newTargetState);
		String newGuard = getAfterGuard(oldTransition);
		if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
			this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, 
					OutputTarget.BOTH, Messages.copiedTransitionNotAdjusted(oldTransition, newTargetState, newPermission)));
		}
	}
	
	private void processPermissionCopy(final CopyElement copyElement) {
		State receivingState = (State) copyElement.getReceivingElement();
		String newPermission = getAfterPermission(receivingState);
		for (Transition incomingTransition : receivingState.getIncomings()) {
			String newGuard = getAfterGuard(incomingTransition);
			if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
				this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
						Messages.copiedPermissionGuardNotAdjusted(receivingState, newPermission, incomingTransition)));
			}
		}
	}
	
	private void processGuardCopy(final CopyElement copyElement) {
		Transition receivingTransition = (Transition) copyElement.getReceivingElement();
		State authState = (State) receivingTransition.getTarget();
		String newPermission = getAfterPermission(authState);
		String newGuard = getAfterGuard(receivingTransition);
		if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
			this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.copiedGuardIncorrect(receivingTransition)));
		}
	}
	
	private void processTransitionEdit(final EditElement editElement) {
		Transition editedTransition = (Transition) editElement.getTarget();
		String targetValue = getElementName(editElement.getValues(),"target");
		if (targetValue != null) {
			State newTargetState = null;
			try {
				newTargetState = UMLHelper.getElementOfNameAndType(this.currentModel, targetValue, State.class);
			} catch (ModelElementNotFoundException e) {
				// TODO: catch
			}
			String newPermission = getAfterPermission(newTargetState);
			String newGuard = getAfterGuard(editedTransition);
			if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
				this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.editedTransitionWrongGuard(editedTransition)));
			}
		}
	}
	
	private void processConstraintEdit(final EditElement editElement) {
		Constraint editedConstraint = (Constraint) editElement.getTarget();
		Transition editedTransition = (Transition) editedConstraint.getOwner();
		State targetState = (State) editedTransition.getTarget();
		String newPermission = getAfterPermission(targetState);
		String newGuard = getAfterGuard(editedTransition);
		if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
			this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.editedGuardWrong(editedTransition)));
		}
	}
	
	private void processPermissionEdit(final EditElement editElement) {
		TaggedValue permissionTag = (TaggedValue) editElement.getTarget();
		State authState = (State) permissionTag.getCorrespondingApplication().getExtendedElement();
		String newPermission = getAfterPermission(authState);
		for (Transition incomingTransition : authState.getIncomings()) {
			String newGuard = getAfterGuard(incomingTransition);
			if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
					this.errorMessages.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH,
							Messages.editedPermissionTransitionNotAdjusted(incomingTransition))); 
			}
		}
	}

	private void processGuardDeletion(final DelElement delElement) {
		Constraint constraint = (Constraint) delElement.getTarget();
		OpaqueExpression guardExpression = UMLStateMachineHelper.getGuard(constraint);
		Transition owningTransition = UMLStateMachineHelper.getTransition(guardExpression);
		if (this.processedIncomings.containsKey(owningTransition)) {
			return;
		}
		State targetState = UMLStateMachineHelper.getTargetState(owningTransition);
		this.processedIncomings.put(owningTransition, targetState);
		if (this.activeDelta.removes(targetState)) {
			return;
		}
		String newGuard = getAfterGuard(owningTransition);
		String permission = getAfterPermission(targetState);
		if (permission != null && newGuard != null && !permission.isEmpty() && !newGuard.equals(permission)) {
			this.errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH,
					Messages.deletedNecessaryGuard(owningTransition, guardExpression, newGuard, permission)));
		}
	}

	
	private void processTransitionAddition(final AddElement addElement) {
		String sourceString = getElementName(addElement.getValues(),"source");
		String targetString = getElementName(addElement.getValues(),"target");
		State targetState = null;
		try {
			targetState = UMLHelper.getElementOfNameAndType(this.currentModel, targetString, State.class);
		} catch (ModelElementNotFoundException e) {
			// TODO: catch and report
		}
		if (targetState != null) {
			String newPermission = getAfterPermission(targetState);
			String newGuard = "";
			for (AddElement containedElement : addElement.getContent()) {
				if (containedElement.getMetaClass().equals(getConstraintClass())) {
					newGuard = getElementName(containedElement.getValues(),"specification");
					break;
				}
			}
			if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
				this.errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedTransitionWrongGuard(targetState, sourceString, newPermission, newGuard)));
			}
		}
	}
	
	private void processPermissionAddition(final AddElement addElement) {
		StereotypeApplication authApp = (StereotypeApplication) addElement.getTarget();
		State authState = (State) authApp.getExtendedElement();
		String newPermission = getAfterPermission(authState);
		for (Transition incomingTransition : authState.getIncomings()) {
			String newGuard = getAfterGuard(incomingTransition);
			if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
				this.errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.newPermissionWrongGuard(incomingTransition, 
						        authState, newPermission, AuthorizedStatus.getGuardString(incomingTransition))));
			}
		}
	}
	
	private void processGuardSubstitution(final SubstElement substElement) {
		Constraint constraint = (Constraint) substElement.getTarget();
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(constraint);
		Transition incomingTransition = UMLStateMachineHelper.getTransition(guard);
		State authorizedState = UMLStateMachineHelper.getTargetState(incomingTransition);
		String newPermission = getAfterPermission(authorizedState);
		String newGuard = getAfterGuard(incomingTransition);
		if (newPermission != null && newGuard != null && !newPermission.isEmpty() && !newGuard.equals(newPermission)) {
			this.errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH,
					Messages.substitutedGuardNotAppropriate(incomingTransition, authorizedState, guard, newPermission)));
		}
	}
	
	/**
	 * Returns the permission string the state someState has after applying the activeDelta to it.
	 * @param someState - the state to check
	 * @return - the permission string if present, an empty string otherwise
	 */
	private String getAfterPermission(final State someState) {
		String permission = "";
		if (this.activeDelta.removes(someState)) {
			return "";
		}
		StereotypeApplication authApp = UMLsecUtil.getStereotypeApplication(someState, UMLsec.AUTHORIZED_STATUS);
		TaggedValue permissionTag = null;
		if (authApp != null) {
			permissionTag = authApp.getTaggedValue(PERMISSION);
			if (permissionTag != null) {
				permission = (String) permissionTag.getValue();
				List<EditElement> edits = this.activeDelta.getEdits(permissionTag);
				if (!edits.isEmpty()) {
					if (edits.size() == 1) {
						return getElementName(edits.get(0).getValues(),"value");
					} 
					// something is wrong, non-deterministic behavior, 2 edits changing the same value
					//TODO: handle this
				}
			} else {
				AddElement addsNewPermission = new AddElement(authApp, getPropertyClass(), null);
				addsNewPermission.addKeyValuePair("name", PERMISSION);
				List<AddElement> matchingAdditions = this.activeDelta.getMatchingAdditions(addsNewPermission);
				matchingAdditions = this.activeDelta.getMatchingAdditions(addsNewPermission);
				if (!matchingAdditions.isEmpty()) {
					AddElement permissionAdd = matchingAdditions.get(0);
					return (String) permissionAdd.getValues().get("value");
				}
			}
		} else {
			AddElement addsNewAuthApp = new AddElement(someState, getStereotypeClass(), null);
			addsNewAuthApp.addKeyValuePair("name", UMLsec.AUTHORIZED_STATUS.toString());
			List<AddElement> matchingAdditions = this.activeDelta.getMatchingAdditions(addsNewAuthApp);
			if (!matchingAdditions.isEmpty()) {
				AddElement matchingAddition = matchingAdditions.get(0);
				if (!matchingAddition.getContent().isEmpty()) {
					AddElement permissionAdd = matchingAddition.getContent().get(0);
					return (String) permissionAdd.getValues().get("value");
				}
			}
		}
		return permission;
	}
	
	private String getAfterGuard(final Transition someTransition) {
		if (this.activeDelta.removes(someTransition)) {
			return "";
		}
		Constraint constraint = someTransition.getGuard();
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(someTransition);
		if (constraint != null) {
			if (!this.activeDelta.removes(constraint) && !this.activeDelta.edits(constraint)) {
				return guard.stringValue();
			}
			if (this.activeDelta.edits(constraint)) {
				return getElementName(this.activeDelta.getEdits(constraint).get(0).getValues(),"specification");
			}
			if (this.activeDelta.removes(constraint)) {
				AddElement addsNewGuard = new AddElement(someTransition, getConstraintClass(), null);
				List<AddElement> matchingAdditions = this.activeDelta.getMatchingAdditions(addsNewGuard);
				if (!matchingAdditions.isEmpty()) {
					AddElement match = matchingAdditions.get(0);
					return getElementName(match.getValues(),"specification");
				}
			}
		} else {
			AddElement addsConstraint = new AddElement(someTransition,getConstraintClass(),null);
			List<AddElement> matches = this.activeDelta.getMatchingAdditions(addsConstraint);
			if (matches.isEmpty()) {
				return "";
			}
			AddElement match = matches.get(0);
			return getElementName(match.getValues(),"specification");
		}
		return "";
	}
	
	private static EClass getStereotypeClass() {
		EClass stereotypeClass = null;
		try {
			stereotypeClass = UMLHelper.getMetaClass("Stereotype");
		} catch (InvalidMetaclassException e) {
			// TODO: catch me if you can
		}
		return stereotypeClass;
	}

	private static EClass getPropertyClass() {
		EClass propertyClass = null;
		try {
			propertyClass = UMLHelper.getMetaClass("Property");
		} catch (InvalidMetaclassException e) {
			// TODO: catch me if you can
		}
		return propertyClass;
	}
	
	private static EClass getConstraintClass() {
		EClass constraintClass = null;
		try {
			constraintClass = UMLHelper.getMetaClass("Constraint");
		} catch (InvalidMetaclassException e) {
			// TODO: catch me if you can
		}
		return constraintClass;
	}
	/**
	 * Util method to get the UML Model element changed by a delta element.
	 * @param de - the changing delta element
	 * @return - the changed model element
	 */
	private static Element getChangedModelElement(final DeltaElement de) {
		EObject target = de.getTarget();
		if (de instanceof AddElement) {
			AddElement add = (AddElement) de;
			AdditiveElement parent = add.getParent();
			while (parent != null) {
				if (parent instanceof SubstElement && parent.getTarget() != null) {
					target = add.getParent().getTarget();
					break;
				} else if (parent instanceof AddElement) {
					AddElement parentAdd = (AddElement) parent;
					if (parentAdd.getParent() == null && parentAdd.getTarget() != null) {
						target = parentAdd.getTarget();
						break;
					}
					parent = parentAdd.getParent();
				}
			}
		}
		if (target != null) {
			if (target instanceof Element) {
				return (Element) target;
			}
			if (target instanceof StereotypeApplication) {
				StereotypeApplication app = (StereotypeApplication) target;
				return app.getExtendedElement();
			}
			if (target instanceof TaggedValue) {
				TaggedValue tv = (TaggedValue) target;
				return tv.getElement();
			}
		}
		return null;
	}
	
	/**
	 * Returns the element name of the element in the value map with the given key.
	 * @param values - the value map
	 * @param key - the key of the wanted element
	 * @return - the name of the element
	 */
	public static String getElementName(final Map<String, Object> values, final String key) {
		Object value = values.get(key);
		if (value instanceof String) {
			return (String) value;
		} else if (value instanceof NamedElement) {
			NamedElement element = (NamedElement) value;
			return element.getName();
		} else {
			return null;
		}
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
