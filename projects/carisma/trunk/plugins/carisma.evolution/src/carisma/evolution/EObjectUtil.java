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
package carisma.evolution;
//TODO: Redundant (also in carisma.core.util), to be moved 

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;

public final class EObjectUtil {

	/**
	 * Hide constructor.
	 */
	private EObjectUtil() {
	}
	
	public static String getName(final EObject obj) {
		if (obj != null) {
			if (obj.getClass().getName().equalsIgnoreCase("StereotypeApplication") || obj.getClass().getName().equalsIgnoreCase("TaggedValue")) {
				return "[no named element]";
			}
			EStructuralFeature sf = obj.eClass().getEStructuralFeature("name");
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("Name");
			}
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature("NAME");
			}
			if (sf != null) {
				return String.valueOf(obj.eGet(sf));
			}
			return "[unnamed " + obj.eClass().getName() + "]";
		}
		return "[no element]";
	}

	public static String getTypeAndName(final EObject obj) {
		if (obj != null) {
			if (obj.getClass().getName().equalsIgnoreCase("StereotypeApplication") || obj.getClass().getName().equalsIgnoreCase("TaggedValue")) {
				return obj.toString();
			}
			return obj.eClass().getName() + " '" + getName(obj) + "'";
		}
		return "[no element]";
	}
}
