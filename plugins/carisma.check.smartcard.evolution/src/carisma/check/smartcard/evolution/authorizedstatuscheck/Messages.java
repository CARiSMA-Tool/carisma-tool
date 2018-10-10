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

import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.check.smartcard.authorizedstatus.AuthorizedStatus;
import carisma.check.smartcard.authorizedstatus.AuthorizedStatusHelper;
import carisma.core.util.EObjectUtil;
import carisma.modeltype.uml2.UMLStateMachineHelper;


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
	
	static String deletedNecessaryGuard(
			final Transition transition,
			final OpaqueExpression guard,
			final String permission) {
		return "The deleted guard '"
				+ guard.stringValue()
				+ " was necessary to uphold the authorized-status of the "
				+ EObjectUtil.getTypeAndName(transition.getTarget())
				+ " containing permission '"
				+ permission
				+ "'.";
	}
	
	static String deletedNecessaryGuard(
			final Transition transition,
			final OpaqueExpression guard,
			final String newGuard,
			final String permission) {
		return "The deleted guard '"
				+ guard.stringValue()
				+ "' was replaced (new Guard: '"
				+ newGuard
				+ "')"
				+ " which doesn't uphold the authorized-status of the "
				+ EObjectUtil.getTypeAndName(transition.getTarget())
				+ " containing permission '"
				+ permission
				+ "'.";
	}
	
	static String addedTransitionWrongGuard(
			final State targetState,
			final String sourceState, 
			final String newPermission,
			final String newGuard) {
		String guardPart = "";
		if (newGuard == null || newGuard.isEmpty()) {
			guardPart = " has no guard, but needs one with permission '";
		} else {
			guardPart = " has guard '" + newGuard + "' without the necessary permission '";
		}
		return "The new transition targeting "
				+ EObjectUtil.getTypeAndName(targetState)
				+ " with source State '"
				+ sourceState
				+ "'"
				+ guardPart
				+ newPermission
				+ "'.";
	}
	
	static String newPermissionWrongGuard(
			final Transition incomingTransition,
			final State authorizedState,
			final String permission,
			final String guardString) {
		return "The new permission '"
				+ permission
				+ "' at "
				+ EObjectUtil.getTypeAndName(authorizedState)
				+ "is not met by the "
				+ EObjectUtil.getTypeAndName(incomingTransition)
				+ " respectively by its guard '"
				+ guardString
				+ "'.";
	}
	
	static String substitutedGuardNotAppropriate(
			final Transition incomingTransition,
			final State authorizedState,
			final OpaqueExpression guard,
			final String permission) {
		return "The guard substitute '"
				+ guard.stringValue()
				+ "' on "
				+ EObjectUtil.getTypeAndName(incomingTransition)
				+ " is not appropriate for permission '"
				+ permission
				+ "' at authorized "
				+ EObjectUtil.getTypeAndName(authorizedState)
				+ ".";
	}
	
	static String copiedTransitionNotAdjusted(
			final Transition newTransition,
			final State targetState, 
			final String permission) {
		return "The copied "
				+ EObjectUtil.getTypeAndName(newTransition)
				+ " with target "
				+ EObjectUtil.getTypeAndName(targetState)
				+ " does not have a guard containing permission '"
				+ permission
				+ "'";
	}

	static String copiedPermissionGuardNotAdjusted(
			final State receivingState, 
			final String copiedPermission, 
			final Transition incomingTransition) {
		return "After copying permission '"
				+ copiedPermission
				+ "'"
				+ " the incoming "
				+ EObjectUtil.getTypeAndName(incomingTransition)
				+ " of the receiving "
				+ EObjectUtil.getTypeAndName(receivingState)
				+ " was not adjusted accordingly.";
	}

	static String copiedGuardIncorrect(final Transition receivingTransition) {
		OpaqueExpression copiedGuard = UMLStateMachineHelper.getGuard(receivingTransition);
		return "Copied the guard "
				+ copiedGuard
				+ " to the receiving "
				+ EObjectUtil.getTypeAndName(receivingTransition)
				+ " without adjusting to the permission '"
				+ AuthorizedStatusHelper.getPermission(receivingTransition)
				+ "'.";
	}

	static String editedTransitionWrongGuard(final Transition editedTransition) {
		return "Edited the "
				+ EObjectUtil.getTypeAndName(editedTransition)
				+ " without adjusting the guard '"
				+ AuthorizedStatus.getGuardString(editedTransition)
				+ "' to permission '"
				+ AuthorizedStatusHelper.getPermission(editedTransition)
				+ "'.";
	}

	static String editedGuardWrong(final Transition editedGuardTransition) {
		return "Edited the guard '"
				+ AuthorizedStatus.getGuardString(editedGuardTransition)
				+ "' at "
				+ EObjectUtil.getTypeAndName(editedGuardTransition)
				+ " without adhering to permission '"
				+ AuthorizedStatusHelper.getPermission(editedGuardTransition)
				+ "'.";
	}

	static String editedPermissionTransitionNotAdjusted(
			final Transition incomingTransition) {
		return "Edited permission at " 
				+ EObjectUtil.getTypeAndName(incomingTransition.getTarget())
				+ " to '"
				+ AuthorizedStatusHelper.getPermission(incomingTransition)
				+ "'"
				+ " but did not adjust Transition guard '"
				+ AuthorizedStatus.getGuardString(incomingTransition)
				+ "'.";
	}
}