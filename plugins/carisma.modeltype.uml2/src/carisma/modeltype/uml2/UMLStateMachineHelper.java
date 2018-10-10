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
package carisma.modeltype.uml2;

import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;
import org.eclipse.uml2.uml.Vertex;

public final class UMLStateMachineHelper {
	
	/**
	 * Hiding constructor.
	 */
	private UMLStateMachineHelper() {
		
	}
	
	public static State getTargetState(final Transition t) {
		Vertex target = t.getTarget();
		if (target instanceof State) {
			return (State) target;
		}
		return null;
	}
	
	public static Transition getTransition(final OpaqueExpression guardExpression) {
		if (guardExpression != null) {
			Element guardOwner = guardExpression.getOwner();
			if (guardOwner instanceof Constraint) {
				Element constraintOwner = guardOwner.getOwner();
				if (constraintOwner instanceof Transition) {
					return (Transition) constraintOwner;
				}
			}
		}
		return null;
	}
	
	public static State getTargetState(final OpaqueExpression guardExpression) {
		Transition owningTransition = getTransition(guardExpression);
		if (owningTransition != null) {
			Vertex targetVertex = owningTransition.getTarget();
			if (targetVertex instanceof State) {
				return (State) targetVertex;
			}
		}
		return null;
	}
	
	public static OpaqueExpression getGuard(final Constraint constraint) {
		if (constraint != null) {
			for (Element ownedElement : constraint.getOwnedElements()) {
				if (ownedElement instanceof OpaqueExpression) {
					return (OpaqueExpression) ownedElement;
				}
			}
		}
		return null;
	}
	
	public static OpaqueExpression getGuard(final Transition transition) {
		if (transition != null) {
			Constraint transitionConstraint = transition.getGuard();
			if (transitionConstraint != null) {
				return getGuard(transitionConstraint);
			}
		}
		return null;
	}
}
