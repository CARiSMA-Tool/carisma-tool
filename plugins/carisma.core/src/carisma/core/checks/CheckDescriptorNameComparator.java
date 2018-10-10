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
package carisma.core.checks;

import java.util.Comparator;

/**
 * A Comparator for the CheckDescriptor names.
 */
public class CheckDescriptorNameComparator implements Comparator<CheckDescriptor> {

	@Override
	public int compare(final CheckDescriptor cd1, final CheckDescriptor cd2) {
		return cd1.getName().compareToIgnoreCase(cd2.getName());
	}
	
}
