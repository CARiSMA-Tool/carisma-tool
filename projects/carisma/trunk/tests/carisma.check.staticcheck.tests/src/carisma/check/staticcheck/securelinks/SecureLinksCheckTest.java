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
package carisma.check.staticcheck.securelinks;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;


import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.junit.Test;

import carisma.check.staticcheck.securelinks.SecureLinksHelper;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.UML2ModelLoader;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * To test the UMLsec implementation.
 * @author Daniel Warzecha
 *
 */
public class SecureLinksCheckTest {
	private String filepath = "resources/models/secure_links";
		
	private UML2ModelLoader ml = null;
	
	private Resource modelres = null;
	
	private Model model = null;
	
	public final void loadModel(final String testmodelname) {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (this.ml == null) {
			this.ml = new UML2ModelLoader();
		}
		try {
			this.modelres = this.ml.load(testmodelfile);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail(e.getMessage());
		}
		assertNotNull(this.modelres);
		this.model = (Model) this.modelres.getContents().get(0);
		assertNotNull(this.model);
	}
	
	@Test
	public final void testRequirements() {
		loadModel("testRequirements.uml");
		NamedElement ne = this.model.getMember("pkg");
		assertNotNull(ne);
		Package pkg = (Package) ne;
		ne = pkg.getMember("dep");
		Dependency dep = (Dependency) ne;
		assertEquals(4, dep.getAppliedStereotypes().size());
		StereotypeApplication requirementApp = UMLsecUtil.getStereotypeApplication(dep, UMLsec.HIGH);
		assertNotNull(requirementApp);
		assertTrue(SecureLinksHelper.isSecureLinksRequirement(requirementApp.getAppliedStereotype()));
		requirementApp = UMLsecUtil.getStereotypeApplication(dep, UMLsec.SECRECY);
		assertNotNull(requirementApp);
		assertTrue(SecureLinksHelper.isSecureLinksRequirement(requirementApp.getAppliedStereotype()));
		requirementApp = UMLsecUtil.getStereotypeApplication(dep, UMLsec.INTEGRITY);
		assertNotNull(requirementApp);
		assertTrue(SecureLinksHelper.isSecureLinksRequirement(requirementApp.getAppliedStereotype()));
		requirementApp = UMLsecUtil.getStereotypeApplication(dep, UMLsec.CALL);
		assertNotNull(requirementApp);
		assertFalse(SecureLinksHelper.isSecureLinksRequirement(requirementApp.getAppliedStereotype()));
		this.modelres.unload();
	}
	
	@Test
	public final void testCheckAttacker() {
		loadModel("testRequirements.uml");
		assertEquals("insider",SecureLinks.getAttacker(this.model));
		NamedElement ne = this.model.getMember("pkg");
		assertNotNull(ne);
		Package pkg = (Package) ne;
		ne = pkg.getMember("aNode");
		assertEquals("custom",SecureLinks.getAttacker(ne));
		ne = pkg.getMember("bNode");
		assertEquals("custom",SecureLinks.getAttacker(ne));
		ne = pkg.getMember("dep");
		assertEquals("custom",SecureLinks.getAttacker(ne));
	}
	
	@Test
	public final void testCheckWrongLinktype() {
		loadModel("testDeploymentWrongLinktype.uml");
		SecureLinks theCheck = new SecureLinks();
		assertEquals(1, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckRightLinktype() {
		loadModel("testDeploymentRightLinktype.uml");
		SecureLinks theCheck = new SecureLinks();
		assertEquals(0, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckWrongCustomMultipleRequirements() {
		loadModel("testDeploymentWrongCustomMultipleRequirements.uml");
		SecureLinks theCheck = new SecureLinks();
		assertEquals(1, theCheck.checkSecureLinks(this.model));
	}
	
	@Test
	public final void testCheckNonUMLsecStereotypes() {
		loadModel("testDeploymentNonUMLsecStereotype.uml");
		SecureLinks theCheck = new SecureLinks();
		assertEquals(0, theCheck.checkSecureLinks(this.model));
	}
	
}
