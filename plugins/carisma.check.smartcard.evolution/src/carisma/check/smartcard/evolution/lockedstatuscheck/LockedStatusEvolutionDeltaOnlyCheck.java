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
import carisma.core.checks.CarismaCheckWithID;
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


public class LockedStatusEvolutionDeltaOnlyCheck implements CarismaCheckWithID {

	private static final String CHECK_ID = "carisma.check.smartcard.evolution.lockedstatusdeltaonlycheck";
	private static final String PRECONDITION_DELTAS_REGISTER_KEY = "carisma.data.evolution.deltas";
	private static final String CHECK_NAME = "Evolution-aware Locked Status Check (Delta Only version)";

	private AnalysisHost analysisHost;
	
	private DeltaList deltaList = null;
	
	private Model currentModel = null;
	
	private Delta activeDelta = null;
	
	private List<AnalysisMessage> errorMessages = null;
	
	private List<DeltaElement> processedDeltaElements = null;
	private Map<State, DeltaElement> processedStates = null;
	
	/** Key unqualified Name; Value List of states with this name.
	*/
	private Map<String, List<State>> stateMapping = new HashMap<>();
	
	/**
	 * Boolean whether to remove the violated models or not.
	 * Normally you choose to delete them, but need to keep them for performance test.
	 * @author Klaus Rudack
	 */
	private Boolean removeViolations = Boolean.TRUE;
	
	public LockedStatusEvolutionDeltaOnlyCheck() {
		this.processedStates = new HashMap<>();
		this.errorMessages = new ArrayList<>();
	}
	
	/**
	 * constructor.
	 * @author Klaus Rudack
	 * @param removeViolations false if  the violating changes should remain, false otherwise
	 */
	public LockedStatusEvolutionDeltaOnlyCheck(final Boolean removeViolations) {
		this.processedStates = new HashMap<>();
		this.errorMessages = new ArrayList<>();
		if (removeViolations != null) {
			this.removeViolations = removeViolations;
		}
	}
	
	@Override
	public boolean perform(Map<String, CheckParameter> parameters, AnalysisHost host) {
		this.analysisHost = host;
		Resource currentModelRes = this.analysisHost.getAnalyzedModel();
		if (currentModelRes.getContents().isEmpty()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Empty model"));
			return false;
		}
		if (!(currentModelRes.getContents().get(0) instanceof Model)) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.WARNING, "Content is not a model!"));
			return false;
		}
		this.currentModel = (Model) currentModelRes.getContents().get(0);
		this.stateMapping.clear();
		for (State s : UMLHelper.getAllElementsOfType(this.currentModel, State.class)) {
			if (this.stateMapping.get(s.getName()) == null) {
				this.stateMapping.put(s.getName(), new ArrayList<State>());
			}
			this.stateMapping.get(s.getName()).add(s);
		}
		try {
			this.deltaList = (DeltaList) this.analysisHost.getFromRegister(PRECONDITION_DELTAS_REGISTER_KEY);
		} catch (RegisterNotInUseException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			return false;
		}
		if (this.deltaList == null) {
			return false;
		}
		if (this.deltaList.isEmpty()) {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No delta left to analyze."));
			return true;
		}
		int beforeMaxChanges = this.deltaList.getHighestChangeCountNow();
		boolean isSuccessful = checkDeltas();
		if (isSuccessful) {
			this.analysisHost.addResultMessage(
					new AnalysisResultMessage(
							StatusType.INFO,
							"A successful maximum Delta (using " + this.deltaList.getHighestChangeCountNow() + " changes) exists."));
		} else {
			this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "No successful maximum Delta (" + beforeMaxChanges + " changes) found."));
			if (this.deltaList.getHighestChangeCountNow() == 0) {
				this.analysisHost.addResultMessage(new AnalysisResultMessage(StatusType.ERROR, "All Deltas violate <<locked-status>>."));
			} else {
				this.analysisHost.addResultMessage(
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
				errorMessage.print(this.analysisHost, "Delta " + deltaCounter + ":");
			}
			if (deltaSuccessful 
					&& d.getNumberOfUsedChanges() == this.deltaList.getHighestChangeCountNow()) {
				hasMaxSuccessfulDelta = true;
			}
			deltaCounter++;
		}
		// KR: request if the violated deltas should be remove, need to keep them for performance test
		if (this.removeViolations.booleanValue()) {
			this.deltaList.removeAll(violatingEvolutions);
		}
		return hasMaxSuccessfulDelta;
	}
	
	private void init() {
		if (this.errorMessages == null) {
			this.errorMessages = new ArrayList<>();
		}
		this.errorMessages.clear();
		if (this.processedDeltaElements == null) {
			this.processedDeltaElements = new ArrayList<>();
		}
		this.processedDeltaElements.clear();
		if (this.processedStates == null) {
			this.processedStates = new HashMap<>();
		}
		this.processedStates.clear();
	}
	
	private void checkActiveDelta() {
		init();
		processAddElements(this.activeDelta.getAllAdditions());
		processSubstElements(this.activeDelta.getAllSubstitutions());
		processEditElements(this.activeDelta.getAllEdits());
		processCopyElements(this.activeDelta.getAllCopies());
	}
		
	private void processAddElements(final List<AddElement> allAdditions) {
		for (AddElement addElement : allAdditions) {
			processAddElement(addElement);
			this.processedDeltaElements.add(addElement);
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
			this.processedDeltaElements.add(editElement);
		}
	}
	
	private void processCopyElements(final List<CopyElement> allCopies) {
		for (CopyElement copyElement : allCopies) {
			processCopyElement(copyElement);
			this.processedDeltaElements.add(copyElement);
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
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
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
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
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
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
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
			List<State> possibleStates = this.stateMapping.get(parts.get(parts.size() - 1));
			for (State possibleState : possibleStates) {
				if (possibleState.getQualifiedName().endsWith(sourceName)) {
					sourceState = possibleState;
					break;
				}
			}
		} else {
			sourceState = (State) sourceValue;
		}
		if (sourceState != null && !this.processedStates.containsKey(sourceState)) {
			StereotypeApplication lockedStatusApp = UMLsecUtil.getStereotypeApplication(sourceState, UMLsec.LOCKED_STATUS);
			if (lockedStatusApp != null) {
				if (!this.activeDelta.removes(lockedStatusApp)) {
					this.errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.addedForbiddenTransition(sourceState)));
				}
			} else {			
				EClass stereotypeMetaclass = null;
				try {
					stereotypeMetaclass = UMLHelper.getMetaClass("Stereotype");
				} catch (InvalidMetaclassException e) {
					Logger.log(LogLevel.ERROR, e.getMessage(), e);
				}
				AddElement addedLockedStatus = new AddElement(sourceState, stereotypeMetaclass, null);
				addedLockedStatus.addKeyValuePair("name", UMLsec.LOCKED_STATUS.toString());
				if (this.activeDelta.adds(addedLockedStatus)) {
					this.errorMessages.add(new AnalysisMessage(
							StatusType.ERROR,
							OutputTarget.BOTH,
							Messages.addedForbiddenTransition(sourceState)));								
				}
			}
			this.processedStates.put(sourceState, addElement);
		}
	}
	
	private void processLockedStatusAddition(final AddElement addElement) {
		State lockedState = (State) addElement.getTarget();
		if (lockedState != null && !this.processedStates.containsKey(lockedState)) {
			List<Transition> remainingOutgoingTransitions = new ArrayList<>();
			remainingOutgoingTransitions.addAll(lockedState.getOutgoings());
			for (Transition outgoingTransition: lockedState.getOutgoings()) {
				if (this.activeDelta.removes(outgoingTransition)) {
					remainingOutgoingTransitions.remove(outgoingTransition);
				}
			}
			List<AddElement> newOutgoingTransitions = new ArrayList<>();
			EClass transitionMetaclass = null;
			try {
				transitionMetaclass = UMLHelper.getMetaClass("Transition");
			} catch (InvalidMetaclassException e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
			AddElement addedOutgoingTransition = new AddElement(null, transitionMetaclass, null);
			addedOutgoingTransition.addKeyValuePair("source", lockedState.getName());
			newOutgoingTransitions.addAll(this.activeDelta.getMatchingAdditions(addedOutgoingTransition));
			if (!remainingOutgoingTransitions.isEmpty()) {
				this.errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedLockedStatusWithoutDeletingOutgoingTransitions(lockedState)));
			}
			if (!newOutgoingTransitions.isEmpty()) {
				this.errorMessages.add(new AnalysisMessage(
						StatusType.ERROR,
						OutputTarget.BOTH,
						Messages.addedForbiddenTransition(lockedState)));				
			}
			this.processedStates.put(lockedState, addElement);
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