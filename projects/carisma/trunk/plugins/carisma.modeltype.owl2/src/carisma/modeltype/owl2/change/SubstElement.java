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

public class SubstElement extends ChangeElement {

	private AddElement addElement;
	private DelElement delElement;
	
	public SubstElement(AddElement addElement, DelElement delElement) {
		this.addElement = addElement;
		this.delElement = delElement;
	}

	public AddElement getAddElement() {
		return addElement;
	}

	public DelElement getDelElement() {
		return delElement;
	}
	
	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer();
		if (addElement != null && delElement != null) {
			buf.append("SubstElement  ("
					+ addElement.toString()
					+ "; "
					+ delElement.toString()
					+ ")");
		}
		return buf.toString();
	}
}
