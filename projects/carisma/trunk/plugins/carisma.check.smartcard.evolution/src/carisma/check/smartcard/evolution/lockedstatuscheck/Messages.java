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

import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.core.util.EObjectUtil;


final class Messages {
	private Messages() {
		
	}
	
	static String checkSuccessful(final Model model) {
		return "The changes on the "
				+ EObjectUtil.getName(model)
				+ " maintain the security.";

	}
	
	static String checkFailed(final Model model) {
		return "The security of the " 
				+ EObjectUtil.getName(model)
				+ " could not be proven!";	
	}
	
	static String addedForbiddenTransition(
			final Transition newTransition,
			final State sourceState) {
		return "The added "
				+ EObjectUtil.getTypeAndName(newTransition)
				+ " coming from locked "
				+ EObjectUtil.getTypeAndName(sourceState)
				+ " is not allowed to target "
				+ EObjectUtil.getTypeAndName(newTransition.getTarget())
				+ ".";
	}

	static String addedForbiddenTransition(
			final State sourceState) {
		return "The state '"
				+ EObjectUtil.getTypeAndName(sourceState)
				+ " will receive a forbidden transition.";
	}
	

	static String addedLockedStatusWithoutDeletingOutgoingTransitions(
			final State lockedState) {
		StringBuffer transitionBuffer = new StringBuffer();
		for (Transition ot : lockedState.getOutgoings()) {
			if (ot.getTarget() != ot.getSource()) {
				transitionBuffer.append(EObjectUtil.getName(ot));
				transitionBuffer.append(",");
			}
		}
		transitionBuffer.deleteCharAt(transitionBuffer.lastIndexOf(","));
		return "Added <<locked-status>> to "
				+ EObjectUtil.getName(lockedState)
				+ " without deleting the violating outgoing transitions ("
				+ transitionBuffer.toString()
				+ ").";
	}
	
	static String editedTransitionSourceToLockedStatus(final Transition editedTransition) {
		return "The "
				+ EObjectUtil.getTypeAndName(editedTransition)
				+ " was edited. Its source is now the locked "
				+ EObjectUtil.getTypeAndName(editedTransition.getSource())
				+ ".";
	}
	static String copiedTransitionSourceHasLockedStatus(final Transition newTransition) {
		return "The new "
				+ EObjectUtil.getTypeAndName(newTransition)
				+ " has the locked "
				+ EObjectUtil.getTypeAndName(newTransition.getSource())
				+ " as source.";
	}
	static String copiedLockedStatusOutgoingTransitions(final State receivingState) {
		return "The "
				+ EObjectUtil.getTypeAndName(receivingState)
				+ " receiving the <<locked-status>> copy has outgoing transitions.";
	}
}
