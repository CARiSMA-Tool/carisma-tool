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

public class UML2ModelLoaderTest {

	private String filepath = "resources/models/";
	
	private Model model = null;
			
	@Test
	public void testProfileWithoutPrefix() {
		this.model = TestHelper.loadModel(this.filepath, "profileNoPrefix.uml");
		assertNotNull(this.model);
		assertEquals(4, this.model.getAllAppliedProfiles().size());
		for (Profile p : this.model.getAllAppliedProfiles()) {
            Logger.log(LogLevel.INFO, p.toString());
		}
	}
	
	/**	
	 * unloads the model.
	 */
	@After
	public final void unload() {
		if (this.model != null) {
			TestHelper.unloadModel(this.model);
			this.model = null;
		}
	}
	
	
}
