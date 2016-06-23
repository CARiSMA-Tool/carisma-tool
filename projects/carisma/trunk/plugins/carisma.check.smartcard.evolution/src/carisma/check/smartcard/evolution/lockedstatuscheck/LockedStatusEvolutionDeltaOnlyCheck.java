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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

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
import carisma.evolution.AddElement;
import carisma.evolution.CopyElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.EditElement;
import carisma.evolution.SubstElement;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


public class LockedStatusEvolutionDeltaOnlyCheck implements CarismaCheck {

	private static final String DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";

	private AnalysisHost host;
	
	private DeltaList deltaList = null;
	
	private Model currentModel = null;
	
	private Delta activeDelta = null;
	
	private List<AnalysisMessage> errorMessages = null;
	
	private List<DeltaElement> processedDeltaElements = null;
	private Map<State, DeltaElement> processedStates = null;
	
	/** Key unqualified Name; Value List of states with this name.
	*/
	private Map<String, List<State>> stateMapping = new HashMap<String, List<State>>();
	
	/**
	 * Boolean whether to remove the violated models or not.
	 * Normally you choose to delete them, but need to keep them for performance test.
	 * @author Klaus Rudack
	 */
	private Boolean removeViolations = true;
	
	public LockedStatusEvolutionDeltaOnlyCheck() {
		processedStates = new HashMap<State, DeltaElement>();
		errorMessages = new ArrayList<AnalysisMessage>();
	}
	
	/**
	 * constructor.
	 * @author Klaus Rudack
	 * @param removeViolations false if  the violating changes should remain, false otherwise
	 */
	public LockedStatusEvolutionDeltaOnlyCheck(final Boolean removeViolations) {
		processedStates = new HashMap<State, DeltaElement>();
		errorMessages = new ArrayList<AnalysisMessage>();
		if (removeViolations != null) {
			this.removeViolations = removeViolations;
		}
	}
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost newHost) {
		host = newHost;
		Resource currentModelRes = host.getAnalyzedModel();
		if (currentModelRes.getContents().isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModelRes.getContents().get(0) instanceof Model)) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		} else {
			currentModel = (Model) currentModelRes.getContents().get(0);
		}
		stateMapping.clear();
		for (State s : UMLHelper.getAllElementsOfType(currentModel, State.class)) {
			if (stateMapping.get(s.getName()) == null) {
				stateMapping.put(s.getName(), new ArrayList<State>());
			}
			stateMapping.get(s.getName()).add(s);
		}
		try {
			deltaList = (DeltaList) host.getFromRegister(DELTAS_REGISTER_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		if (deltaList == null) {
			return false;
		}
		if (deltaList.isEmpty()) {
			host.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No delta left to analyze."));
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
			activeDelta = d;
			boolean deltaSuccessful = true;
			checkActiveDelta();
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
	}
	
	private void checkActiveDelta() {
		init(activeDelta);
		processAddElements(activeDelta.getAllAdditions());
		processSubstElements(activeDelta.getAllSubstitutions());
		processEditElements(activeDelta.getAllEdits());
		processCopyElements(activeDelta.getAllCopies());
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
				&& ((String) addElement.getValues().get("name")).contains(UMLsec.LOCKED_STATUS.toString())) {
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
			if (UMLsec.LOCKED_STATUS.isEqual(app.getAppliedStereotype())) {
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
		EClass transitionMetaclass = null;
		try {
			transitionMetaclass = UMLHelper.getMetaClass("Transition");
		} catch (InvalidMetaclassException e) {
			
		}
		AddElement addTransition = new AddElement(null, transitionMetaclass, null);
		addTransition.addKeyValuePair("source", editElement.getValues().get("source"));
		processTransitionAddition(addTransition);
	}
	
	private void processTransitionCopy(final CopyElement copyElement) {
		EClass transitionMetaclass = null;
		try {
			transitionMetaclass = UMLHelper.getMetaClass("Transition");
		} catch (InvalidMetaclassException e) {
			
		}
		AddElement addTransition = new AddElement(null, transitionMetaclass, null);
		addTransition.addKeyValuePair("source", copyElement.getChangedValues().get("source"));
		processTransitionAddition(addTransition);
		
	}
	
	private void processLockedStatusCopy(final CopyElement copyElement) {
		EClass stereotypeMetaclass = null;
		try {
			stereotypeMetaclass = UMLHelper.getMetaClass("Stereotype");
		} catch (InvalidMetaclassException e) {
			
		}		
		AddElement addLocked = new AddElement(copyElement.getReceivingElement(), stereotypeMetaclass, null);
		addLocked.addKeyValuePair("name", UMLsec.LOCKED_STATUS.toString());
		processLockedStatusAddition(addLocked);
	}
	
	private void processTransitionAddition(final AddElement addElement) {
		Object sourceValue = addElement.getValues().get("source");
		String sourceName = null;
		State sourceState = null;
		if (sourceValue instanceof String) {
			sourceName = (String) sourceValue;
			List<String> parts = Arrays.asList(sourceName.split("::"));
			List<State> possibleStates = stateMapping.get(parts.get(parts.size() - 1));
			for (State possibleState : possibleStates) {
				if (possibleState.getQualifiedName().endsWith(sourceName)) {
					sourceState = possibleState;
					break;
				}
			}
		} else {
			sourceState = (State) sourceValue;
		}
		if (sourceState != null && !processedStates.containsKey(sourceState)) {
			StereotypeApplication lockedStatusApp = UMLsecUtil.getStereotypeApplication(sourceState, UMLsec.LOCKED_STATUS);
			if (lockedStatusApp != null) {
				if (!activeDelta.removes(lockedStatusApp)) {
					errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.addedForbiddenTransition(sourceState)));
				}
			} else {			
				EClass stereotypeMetaclass = null;
				try {
					stereotypeMetaclass = UMLHelper.getMetaClass("Stereotype");
				} catch (InvalidMetaclassException e) {
					
				}
				AddElement addedLockedStatus = new AddElement(sourceState, stereotypeMetaclass, null);
				addedLockedStatus.addKeyValuePair("name", UMLsec.LOCKED_STATUS.toString());
				if (activeDelta.adds(addedLockedStatus)) {
					errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.addedForbiddenTransition(sourceState)));								
				}
			}
			processedStates.put(sourceState, addElement);
		}
	}
	
	private void processLockedStatusAddition(final AddElement addElement) {
		State lockedState = (State) addElement.getTarget();
		if (lockedState != null && !processedStates.containsKey(lockedState)) {
			List<Transition> remainingOutgoingTransitions = new ArrayList<Transition>();
			remainingOutgoingTransitions.addAll(lockedState.getOutgoings());
			for (Transition outgoingTransition: lockedState.getOutgoings()) {
				if (activeDelta.removes(outgoingTransition)) {
					remainingOutgoingTransitions.remove(outgoingTransition);
				}
			}
			List<AddElement> newOutgoingTransitions = new ArrayList<AddElement>();
			EClass transitionMetaclass = null;
			try {
				transitionMetaclass = UMLHelper.getMetaClass("Transition");
			} catch (InvalidMetaclassException e) {
				
			}
			AddElement addedOutgoingTransition = new AddElement(null, transitionMetaclass, null);
			addedOutgoingTransition.addKeyValuePair("source", lockedState.getName());
			newOutgoingTransitions.addAll(activeDelta.getMatchingAdditions(addedOutgoingTransition));
			if (!remainingOutgoingTransitions.isEmpty()) {
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedLockedStatusWithoutDeletingOutgoingTransitions(lockedState)));
			}
			if (!newOutgoingTransitions.isEmpty()) {
				errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedForbiddenTransition(lockedState)));				
			}
			processedStates.put(lockedState, addElement);
		}
	}
}
