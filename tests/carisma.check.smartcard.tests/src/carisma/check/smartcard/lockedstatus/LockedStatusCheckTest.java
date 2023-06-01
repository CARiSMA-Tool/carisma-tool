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
import org.eclipse.uml2.uml.Model;
import org.junit.Test;

import carisma.check.smartcard.TestHelper;


public class LockedStatusCheckTest {
	
	private static final String filepath = "resources/models/";
	
	/*private ResourceSet rs = new ResourceSetImpl();
	
	private Resource modelres = null;

	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
			ml = new UML2ModelLoader();
		}
		try {
			modelres = ml.load(testmodelfile);
		} catch (IOException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
            fail("Couldn't load Model:" + testmodelname);
		}
	}*/

	@SuppressWarnings("static-method")
	@Test
	public void testLockedStatus() {
		Model model = TestHelper.loadModel(filepath, "testLockedStatus.uml");
		LockedStatus check = new LockedStatus();
		assertEquals(1, check.startCheck(model));
	}

}
