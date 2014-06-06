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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Profile;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.tests.modelutils.uml.TestHelper;

public class UML2ModelLoaderTest {

	private String filepath = "resources/models/";
	
	private Model model = null;
			
	@Test
	public void testProfileWithoutPrefix() {
		model = TestHelper.loadModel(filepath, "profileNoPrefix.uml");
		assertNotNull(model);
		assertEquals(4, model.getAllAppliedProfiles().size());
		for (Profile p : model.getAllAppliedProfiles()) {
            Logger.log(LogLevel.INFO, p.toString());
		}
	}
	
	/**	
	 * unloads the model.
	 */
	@After
	public final void unload() {
		if (model != null) {
			TestHelper.unloadModel(model);
			model = null;
		}
	}
	
	
}
