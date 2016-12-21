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

import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Test;

public class UMLDeploymentHelperTest {
	
	private String filepath = "resources/models/";
	
	private Model model = null;
	
	@Test
	public final void testGetDeploymentLocations() {
		this.model = TestHelper.loadModel(this.filepath, "testDeploymentHelper.uml");
		Package pkg = (Package) this.model.getMember("testDeploymentHelperPackage");
		assertNotNull(pkg);
		Artifact arti1 = (Artifact) pkg.getMember("Artifact1");
		assertNotNull(arti1);
		assertEquals(1, UMLDeploymentHelper.getDeploymentLocations(arti1).size());
		Artifact arti2 = (Artifact) pkg.getMember("Artifact2");
		assertNotNull(arti2);
		assertEquals(2, UMLDeploymentHelper.getDeploymentLocations(arti2).size());
		Artifact arti3 = (Artifact) pkg.getMember("Artifact3");
		assertNotNull(arti3);
		assertEquals(1, UMLDeploymentHelper.getDeploymentLocations(arti3).size());
	}
	
	@Test
	public final void testGetDeployedArtifacts() {
		this.model = TestHelper.loadModel(this.filepath, "testDeploymentHelper.uml");
		Package pkg = (Package) this.model.getMember("testDeploymentHelperPackage");
		assertNotNull(pkg);
		Node node1 = (Node) pkg.getMember("Node1");
		assertNotNull(node1);
		assertEquals(2, UMLDeploymentHelper.getDeployedArtifacts(node1).size());
		Node node2 = (Node) pkg.getMember("Node2");
		assertNotNull(node2);
		assertEquals(2, UMLDeploymentHelper.getDeployedArtifacts(node2).size());
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
