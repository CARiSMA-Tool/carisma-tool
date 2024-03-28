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
package carisma.profile.common;

import org.eclipse.uml2.uml.Stereotype;

public interface UMLsecProfile {
	
	public String getReadableName();
	public boolean isApplicable(Stereotype stereotype);
	
	public UMLsecProfile getValue(String name);
}
