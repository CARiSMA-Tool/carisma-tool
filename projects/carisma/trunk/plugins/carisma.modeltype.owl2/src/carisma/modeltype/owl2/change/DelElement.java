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
package carisma.modeltype.owl2.change;

import org.eclipse.emf.ecore.EObject;

import carisma.core.util.EObjectUtil;

public class DelElement extends DeltaElement {

	public DelElement (EObject target) {
		super(target);
	}
	
	@Override
	public String toString() {
		StringBuilder buf = new StringBuilder();
		if (this.getTarget() != null) {
			buf.append("DelElement (Target: "
					+ EObjectUtil.getTypeAndName(this.getTarget())
					+ ")");
		}
		return buf.toString();
	}
}
