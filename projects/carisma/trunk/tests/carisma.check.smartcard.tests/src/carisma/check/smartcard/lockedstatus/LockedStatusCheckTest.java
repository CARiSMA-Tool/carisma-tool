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
package carisma.check.smartcard.lockedstatus;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Model;
import org.junit.Test;

import carisma.check.smartcard.lockedstatus.LockedStatusCheck;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.tests.modelutils.uml.TestHelper;


public class LockedStatusCheckTest {
	
	private String filepath = "resources/models/";
	
	/*private UML2ModelLoader ml = null;
	
	private Resource modelres = null;

	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new UML2ModelLoader();
		}
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            fail("Couldn't load Model:" + testmodelname);
		}
	}*/

	@Test
	public void testLockedStatus() {
		Model model = TestHelper.loadModel(filepath, "testLockedStatus.uml");
		LockedStatusCheck check = new LockedStatusCheck();
		assertEquals(1, check.startCheck(model));
	}

}
