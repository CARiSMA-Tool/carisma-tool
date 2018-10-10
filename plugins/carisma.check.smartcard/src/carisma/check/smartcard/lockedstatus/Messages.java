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

import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.core.util.EObjectUtil;


public final class Messages {
    /** Hiding default constructor.
     */
	private Messages() {
	}
	
	static String stateHasOutgoingTransitions(final State s, final Transition t) {
		return "Locked "
				+ EObjectUtil.getTypeAndName(s)
				+ " has outgoing "
				+ EObjectUtil.getTypeAndName(t)
				+ " that targets "
				+ EObjectUtil.getTypeAndName(t.getTarget())
				+ ".";
	}
	
	static String allLockedStatesCorrect() {
		return "LockedStatusCheck successful with respect to locked-status";
	}
	
	static String outgoingTransitionFromLockedStatus(
			final Transition outgoingTransition,
			final State sourceState) {
		return "The outgoing "
				+ EObjectUtil.getTypeAndName(outgoingTransition)
				+ " has locked-status "
				+ EObjectUtil.getTypeAndName(sourceState)
				+ " as source and targets "
				+ EObjectUtil.getName(outgoingTransition.getTarget())
				+ ". This is not allowed.";
	}
}
