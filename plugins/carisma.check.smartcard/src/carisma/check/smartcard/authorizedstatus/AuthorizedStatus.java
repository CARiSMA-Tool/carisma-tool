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
package carisma.check.smartcard.authorizedstatus;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Stereotype;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.utils.AnalysisMessage;
import carisma.check.smartcard.utils.OutputTarget;
import carisma.core.analysis.result.StatusType;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UMLStateMachineHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * checks a statemachine with respect to authorized-Status.
 * @author Klaus Rudack
 *
 */
public class AuthorizedStatus {
	private List<AnalysisMessage> errorMessages = null;
	/**
	 * the constructor.
	 */
	public AuthorizedStatus() {
		this.errorMessages = new ArrayList<>();
	}
	
	public List<AnalysisMessage> getErrorMessages() {
		return Collections.unmodifiableList(this.errorMessages);
	}
	
	/**
	 * checks a given statemachine with the respect to authorized-status.
	 * @param model model where the statemachine is in
	 * @param host AnalysisHost for report
	 */	
	public int checkAllAuthorizedStates(final Package pkg) {
		this.errorMessages.clear();
		for (Element e : UMLsecUtil.getStereotypedElements(pkg, UMLsec.AUTHORIZED_STATUS)) {
			State s = (State) e; 
			Stereotype appliedAuthorizedStatus = s.getAppliedStereotype("UMLsec::authorized-status");
			if (appliedAuthorizedStatus != null) {
				String permissionString = (String) s.getValue(appliedAuthorizedStatus, "permission");
				if (permissionString != null && (!permissionString.isEmpty())) {
					this.errorMessages.addAll(checkAuthorizedState(s, permissionString));
				}
			}
		}
		return this.errorMessages.size();
	}
	
	public static List<AnalysisMessage> checkAuthorizedState(final State s, final String permissionString) {
		List<AnalysisMessage> errors = new ArrayList<>();
		if (s != null && permissionString != null && (!permissionString.isEmpty())) {
			for (Transition incomingTransition : s.getIncomings()) {
				errors.addAll(checkIncomingTransition(incomingTransition, permissionString));
			}
		}
		return errors;
	}
	
	public static List<AnalysisMessage> checkIncomingTransition(final Transition incomingTransition) {
		List<AnalysisMessage> errors = new ArrayList<>();
		State targetState = (State) incomingTransition.getTarget();
		StereotypeApplication authApp = UMLsecUtil.getStereotypeApplication(targetState, UMLsec.AUTHORIZED_STATUS);
		if (authApp != null) {
			String permission = (String) authApp.getTaggedValue("permission").getValue();
			errors.addAll(checkIncomingTransition(incomingTransition, permission));
		}
		return errors;
	}
	public static List<AnalysisMessage> checkIncomingTransition(final Transition incomingTransition, final String permissionString) {
		List<AnalysisMessage> errors = new ArrayList<>();
		if (incomingTransition != null && permissionString != null && (!permissionString.isEmpty())) {
			State targetState = (State) incomingTransition.getTarget();
			Constraint transitionConstraint = incomingTransition.getGuard();
			if (transitionConstraint != null) {
				boolean containsPermission = false;
				OpaqueExpression guard = UMLStateMachineHelper.getGuard(transitionConstraint);
				if (guard != null) {
					for (String body : guard.getBodies()) {
						if (body.equals(permissionString)) {
							containsPermission = true;
						}
					}					
				}
				if (!containsPermission) {
					String violation = Messages.incomingTransitionWrongGuard(targetState, incomingTransition, guard, permissionString);
					errors.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, violation));
				}
			} else {
				String violation = Messages.incomingTransitionNotGuarded(targetState, incomingTransition, permissionString);
				errors.add(new AnalysisMessage(StatusType.ERROR, OutputTarget.BOTH, violation));
			}
		}
		return errors;
	}
	
	public static String getPermission(final State authorizedState) {
		if (authorizedState != null) {
			StereotypeApplication authorizedApp = UMLsecUtil.getStereotypeApplication(authorizedState, UMLsec.AUTHORIZED_STATUS);
			if (authorizedApp != null) {
				return (String) authorizedApp.getTaggedValue("permission").getValue();
			}
		}
		return null;
	}
	
	public static String getGuardString(final Transition guardedTransition) {
		if (guardedTransition == null) {
			return null;
		}
		Constraint guardConstraint = guardedTransition.getGuard();
		if (guardConstraint == null) {
			return "";
		}
		OpaqueExpression guard = UMLStateMachineHelper.getGuard(guardConstraint);
		if (guard == null) {
			return "";
		}
		return guard.stringValue();
	}
}
