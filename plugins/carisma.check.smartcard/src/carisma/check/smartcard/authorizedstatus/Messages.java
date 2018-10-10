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

import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;

import carisma.core.util.EObjectUtil;


public final class Messages {
    /** Hiding constructor.
     */
	private Messages() {
		
	}
	
	public static String incomingTransitionWrongGuard(final State s, final Transition incomingTransition, final OpaqueExpression guard, final String permissionString) {
		StringBuffer buf = new StringBuffer();
		buf.append("'");
		for (String body : guard.getBodies()) {
			buf.append(body);
			buf.append("','");
		}
		return EObjectUtil.getTypeAndName(s)
				+ " with incoming "
				+ EObjectUtil.getTypeAndName(incomingTransition)
				+ " has guard bodies "
				+ buf.substring(0, buf.lastIndexOf(",'"))
				+ ", but the necessary permission is '"
				+ permissionString
				+ "'.";
	}
	
	public static String incomingTransitionNotGuarded(final State s, final Transition incomingTransition, final String permissionString) {
		return "Incoming "
				+ EObjectUtil.getTypeAndName(incomingTransition)
				+ " at " 
				+ EObjectUtil.getTypeAndName(s)
				+ " has no guard! It should have a guard with permission '"
				+ permissionString
				+ "'";
	}
	
	public static String checkSuccessful() {
		return "Correct with the respect to authorized-status";
	}
}
