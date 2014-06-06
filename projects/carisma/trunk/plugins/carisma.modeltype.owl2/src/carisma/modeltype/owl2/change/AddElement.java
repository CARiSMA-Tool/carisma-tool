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

public class AddElement extends DeltaElement {

	private EObject additionalElement;
	private ElementType type;
	private String featureName;
	
	public AddElement(EObject target, String featureName, ElementType type, EObject additionalElement) {
		super(target);
		this.additionalElement = additionalElement;
		this.type = type;
		this.featureName = featureName;
	}

	public EObject getAdditionalElement() {
		return additionalElement;
	}
	
	public ElementType getType() {
		return type;
	}
	
	public String getFeatureName() {
		return featureName;
	}
	
	@Override
	public String toString() {
		StringBuilder buf = new StringBuilder();
		if (this.getTarget() != null && this.getType() != null && this.getFeatureName() != null) {
			buf.append("AddElement (Target: "
					+ EObjectUtil.getTypeAndName(this.getTarget())
					+ "; Type: "
					+ this.type.getValue()
					+ "; FeatureName: "
					+ featureName
					+ ")");
		}
		return buf.toString();
	}
}
