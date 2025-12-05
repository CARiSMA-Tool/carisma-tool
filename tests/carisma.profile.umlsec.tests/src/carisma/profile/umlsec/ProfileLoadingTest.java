/*******************************************************************************
 * Copyright (c) 2025 Research Group Software Engineering, University of Koblenz
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {RGSE group} 
 *******************************************************************************/
package carisma.profile.umlsec;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.IOException;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.resource.UMLResource;
import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil;
import org.junit.Test;

/**
 * Tests the UMLsec profile loading within in JUnit tests.
 * 
 * @author Julian Flake
 *
 */
public class ProfileLoadingTest {

	/**
	 * - {@code ./mvnw --projects :carisma.profile.umlsec.tests clean verify}
	 * succeeds with this test
	 * 
	 * - Running this test in Eclipse fails.
	 * 
	 * @throws IOException
	 */
	@Test
	public final void profileLoadingFromModelTest() throws IOException {
		ResourceSet resourceSet = new ResourceSetImpl();
		UMLResourcesUtil.init(resourceSet);
		String path = "resources/models/profileLoading/profileLoadingTest.uml";
		UMLResource resource = (UMLResource) resourceSet.createResource(URI.createFileURI(path));
		assertNotNull(resource);
		resource.load(null);
		assertEquals(7, resource.getContents().size());
		assertNotNull(resource);
		Model model = (Model) resource.getContents().get(0);
		assertNotNull(model);
		assertEquals(1, model.getMembers().size());
		// getApplied Profiles
		Profile profile = model.getAppliedProfiles().get(0);
		assertNotNull(profile);
		// This fails in Eclipse
		assertEquals("http://www.umlsec.de/profiles/UMLsec", profile.getURI());
	}

}
