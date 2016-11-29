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
package carisma.check.smartcard.lockedstatus;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.check.smartcard.utils.OutputTarget;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * Class to check a state machine with respect to locked status.
 * @author Klaus Rudack
 *
 */
public class LockedStatus {

	private List<AnalysisMessage> errorMessages = null;
	
	/**
	 * constructor.
	 */
	public LockedStatus() {
		this.errorMessages = new ArrayList<>();
	}
	
	public List<AnalysisMessage> getErrorMessages() {
		return Collections.unmodifiableList(this.errorMessages);
	}
	
	/**
	 * checks a model with respect to locked status.
	 * @param model the model to test
	 * @param newHost analysisHost for details
	 * @return boolean if check was successful or not
	 */
	public final int startCheck(final Package model) {
		this.errorMessages.clear();
		this.errorMessages.addAll(checkAllLockedStates(model));
		return this.errorMessages.size();
	}
	
	/**
	 * checks all States in the given Model if it has outgoing transition, this will cause the test to fail.
	 * @param model the given Model
	 */
	public final static List<AnalysisMessage> checkAllLockedStates(final Package model) {
		List<AnalysisMessage> errors = new ArrayList<>();
		for (State s : UMLHelper.getAllElementsOfType(model, State.class)) {
			errors.addAll(checkLockedStatus(s));
		}
		return errors;
	}
	
	public static List<AnalysisMessage> checkLockedStatus(final State s) {
		List<AnalysisMessage> errors = new ArrayList<>();
		if (UMLsecUtil.hasStereotype(s, UMLsec.LOCKED_STATUS)) {
			for (Transition t : s.getOutgoings()) {
				if (t.getTarget() != s) {
					errors.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, Messages.stateHasOutgoingTransitions(s, t)));
				}
			}
		}
		return errors;
	}
	
	public static List<AnalysisMessage> checkTransition(final Transition outgoingTransition) {
		List<AnalysisMessage> errors = new ArrayList<>();
		if (outgoingTransition.getSource() instanceof State) {
			State sourceState = (State) outgoingTransition.getSource();
			if ((UMLsecUtil.hasStereotype(sourceState, UMLsec.LOCKED_STATUS)) 
					&& outgoingTransition.getTarget() != sourceState) {
				errors.add(
							new AnalysisMessage(
									StatusType.ERROR,
									OutputTarget.BOTH,
									Messages.outgoingTransitionFromLockedStatus(outgoingTransition, sourceState)));				
			}
		}
		return errors;
	}
}
