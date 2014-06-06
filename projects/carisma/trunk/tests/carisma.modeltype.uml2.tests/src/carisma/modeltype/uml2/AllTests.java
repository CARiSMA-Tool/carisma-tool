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

import junit.framework.Test;
import junit.framework.TestSuite;

public final class AllTests {

	/**
	 * Hide constructor.
	 */
	private AllTests() {
	}
	
	public static Test suite() {
		TestSuite suite = new TestSuite(AllTests.class.getName());
		//$JUnit-BEGIN$
//			suite.addTestSuite(StateMachinePaths.class);
		//$JUnit-END$
		return suite;
	}

}
