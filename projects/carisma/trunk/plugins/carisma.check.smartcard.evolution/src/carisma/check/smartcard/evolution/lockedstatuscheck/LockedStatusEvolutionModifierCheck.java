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
package carisma.check.smartcard.evolution.lockedstatuscheck;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Namespace;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.lockedstatus.LockedStatusCheck;
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
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.ModifierMap;
import carisma.evolution.uml2.UMLModifier;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public class LockedStatusEvolutionModifierCheck implements CarismaCheck {

	private static final String DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";
	private static final String MODIFIERS_REGISTRY_KEY = "carisma.data.evolution.modifiers";

	private AnalysisHost host;
	
	private DeltaList deltaList = null;
	private ModifierMap deltaModifiers = null;
	
	private UMLModifier deltaModifier = null;
	
	private LockedStatusCheck lockedStatusCheck = null;
	
	private List<AnalysisMessage> errorMessages = null;
	
	private List<DeltaElement> processedDeltaElements = null;
	private Map<State, DeltaElement> processedStates = null;
	
	/**
	 * Boolean whether to remove the violated models or not.
	 * Normally you choose to delete them, but need to keep them for performance test.
	 * @author Klaus Rudack
	 */
	private Boolean removeViolations = true;
	
	public LockedStatusEvolutionModifierCheck() {
		processedStates = new HashMap<State, DeltaElement>();
		errorMessages = new ArrayList<AnalysisMessage>();
	}
	
	/**
	 * constructor.
	 * @author Klaus Rudack
	 * @param removeViolations false if  the violating changes should remain, false otherwise
	 */
	public LockedStatusEvolutionModifierCheck(final Boolean removeViolations) {
		processedStates = new HashMap<State, DeltaElement>();
		errorMessages = new ArrayList<AnalysisMessage>();
		if (removeViolations != null) {
			this.removeViolations = removeViolations;
		}
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
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No deltaList left to analyze."));
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
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (" + beforeMaxChanges + " changes) found."));
			if (deltaList.getHighestChangeCountNow() == 0) {
				host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<locked-status>>."));
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
		// KR: request if the violated deltas should be remove, need to keep them for performance test
		if (removeViolations) {
			deltaList.removeAll(violatingEvolutions);
		}
		return hasMaxSuccessfulDelta;
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
		if (processedStates == null) {
			processedStates = new HashMap<State, DeltaElement>();
		}
		processedStates.clear();
		lockedStatusCheck = new LockedStatusCheck();
	}
	
	private void checkDelta(final Delta d) {
		init(d);
		processAddElements(d.getAllAdditions());
		processSubstElements(d.getAllSubstitutions());
		processEditElements(d.getAllEdits());
		processCopyElements(d.getAllCopies());
	}
		
	private void processAddElements(final List<AddElement> allAdditions) {
		for (AddElement addElement : allAdditions) {
			processAddElement(addElement);
			processedDeltaElements.add(addElement);
			processContent(addElement);
		}
	}
	
	private void processSubstElements(final List<SubstElement> allSubstitutions) {
		for (SubstElement substElement : allSubstitutions) {
			for (AddElement component : substElement.getComponents()) {
				processAddElement(component);
			}
		}
	}
	
	private void processEditElements(final List<EditElement> allEdits) {
		for (EditElement editElement : allEdits) {
			processEditElement(editElement);
			processedDeltaElements.add(editElement);
		}
	}
	
	private void processCopyElements(final List<CopyElement> allCopies) {
		for (CopyElement copyElement : allCopies) {
			processCopyElement(copyElement);
			processedDeltaElements.add(copyElement);
		}
	}
	
	private void processAddElement(final AddElement addElement) {
		EClass newElemClass = addElement.getMetaClass();
		if (newElemClass.getName().equalsIgnoreCase("Transition")) {
			processTransitionAddition(addElement);
		} else if (newElemClass.getName().equalsIgnoreCase("Stereotype") 
				&& addElement.getTarget() instanceof State 
				&& ((String) addElement.getValues().get("name")).contains("locked-status")) {
			processLockedStatusAddition(addElement);
		}
	}
	
	private void processEditElement(final EditElement editElement) {
		EObject target = editElement.getTarget();
		if (target instanceof Transition && editElement.getValues().containsKey("source")) {
			processTransitionEdit(editElement);
		}
	}
	
	private void processCopyElement(final CopyElement copyElement) {
		EObject target = copyElement.getTarget();
		if (target instanceof Transition) {
			processTransitionCopy(copyElement);
		} else if (target instanceof StereotypeApplication) {
			StereotypeApplication app = (StereotypeApplication) target;
			if (UMLsec.AUTHORIZED_STATUS.isEqual(app.getAppliedStereotype())) {
				processLockedStatusCopy(copyElement);
			}
		}
	}
	
	private void processContent(final AddElement addElement) {
		for (AddElement containedElement : addElement.getContent()) {
			processAddElement(containedElement);
			processContent(containedElement);
		}
	}
	
	private void processTransitionEdit(final EditElement editElement) {
		EditElement editCopy = (EditElement) deltaModifier.get(editElement);
		Transition editedTransition = (Transition) editCopy.getTarget();
		if (!lockedStatusCheck.checkTransition(editedTransition).isEmpty()) {
			errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH, Messages.editedTransitionSourceToLockedStatus(editedTransition)));
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
		} catch (ModelElementNotFoundException e) {
			errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH, "Couldn't find copy of " + EObjectUtil.getTypeAndName(oldCopiedTransition) + ". Message: "+ e.getMessage()));
		}
		if (!(lockedStatusCheck.checkTransition(newTransition).isEmpty())) {
			errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH, Messages.copiedTransitionSourceHasLockedStatus(newTransition)));
		}
	}
	
	private void processLockedStatusCopy(final CopyElement copyElement) {
		CopyElement copyCopy = (CopyElement) deltaModifier.get(copyElement);
		StereotypeApplication newCopyOfApp = UMLsecUtil.getStereotypeApplication((Element) copyCopy.getReceivingElement(), UMLsec.LOCKED_STATUS);
		if (!lockedStatusCheck.checkLockedStatus((State) newCopyOfApp.getExtendedElement()).isEmpty()) {
			errorMessages.add(new AnalysisMessage(
					StatusType.ERROR,
					OutputTarget.BOTH, Messages.copiedLockedStatusOutgoingTransitions((State) newCopyOfApp.getExtendedElement())));
		}
	}
	
	private void processTransitionAddition(final AddElement addElement) {
		AddElement addCopy = (AddElement) deltaModifier.get(addElement);
		Namespace newTransitionContainer = (Namespace) addCopy.getTarget();
		Transition newTransition = (Transition) newTransitionContainer.getMember((String) addCopy.getValues().get("name"));
		if (newTransition.getSource() instanceof State) {
			State sourceState = (State) newTransition.getSource();
			if (!processedStates.containsKey(sourceState)) {
				processedStates.put(sourceState, addElement);
				if (!lockedStatusCheck.checkTransition(newTransition).isEmpty()) {
					errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.addedForbiddenTransition(newTransition, sourceState)));
				}
			}			
		}
	}
	
	private void processLockedStatusAddition(final AddElement addElement) {
		AddElement addCopy = (AddElement) deltaModifier.get(addElement);		
		State lockedState = (State) addCopy.getTarget();
		if (lockedState != null 
				&& !processedStates.containsKey(lockedState)) {
			processedStates.put(lockedState, addElement);
			if (!lockedStatusCheck.checkLockedStatus(lockedState).isEmpty()) {
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedLockedStatusWithoutDeletingOutgoingTransitions(lockedState)));
			}
		}
	}
}
