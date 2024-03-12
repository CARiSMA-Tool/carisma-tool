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
package carisma.check.staticcheck.securedependency;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Interface;
import org.eclipse.uml2.uml.InterfaceRealization;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Usage;
import org.junit.After;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;


/**
 * To test the implementation of the secure dependency check.
 * @author Sven Wenzel
 *
 */
public class SecureDependencyChecksTest {
	
	private String filepath = "resources/models/secure_dependency";
	
	private Model model = null;
	
	@After
	public final void cleanup() {
		this.model.eResource().unload();
		this.model = null;
	}
	
	/**
	 * This tests whether 
	 */
	@Test
	public final void testSecureDependency1() {
		this.model = loadModel(this.filepath, "testSecureDependency1.uml");
		Package pkg = checkedGetElement(this.model, "testSecureDependency1", Package.class);
		Class class1 = checkedGetElement(pkg, "Class1", Class.class);
		Class class2 = checkedGetElement(pkg, "Class2", Class.class);
		Interface interface1 = checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = checkedGetElement(pkg, "Dep1", Dependency.class);
		InterfaceRealization realization1 = checkedGetElement(pkg, "InterfaceRealization1", InterfaceRealization.class);
		
		assertEquals(UMLHelper.getStereotypeApplications(dependency1).toString(),1, UMLHelper.getStereotypeApplications(dependency1).size());
		assertTrue(UMLHelper.isStereotypeApplied(dependency1, "call"));
		assertEquals(class1, dependency1.getClients().get(0));
		assertEquals(interface1, dependency1.getSuppliers().get(0));
		assertNotNull(UMLsecUtil.getStereotypeApplication(class2, UMLsec.CRITICAL));
		assertEquals(interface1, realization1.getSuppliers().get(0));
		assertEquals(class2, realization1.getClients().get(0));
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	
	@Test
	public final void testSecureDependency2() {
		this.model = loadModel(this.filepath, "testSecureDependency2.uml");
		Package pkg = checkedGetElement(this.model, "testSecureDependency2", Package.class);
		Class class1 = checkedGetElement(pkg, "Class1", Class.class);
		Class class2 = checkedGetElement(pkg, "Class2", Class.class);
		Interface interface1 = checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = checkedGetElement(pkg, "Dep1", Dependency.class);
		
		assertEquals(UMLHelper.getStereotypeApplications(dependency1).toString(), 2, UMLHelper.getStereotypeApplications(dependency1).size());
		assertTrue(UMLHelper.isStereotypeApplied(dependency1, "call"));
		assertEquals(class1, dependency1.getClients().get(0));
		assertEquals(interface1, dependency1.getSuppliers().get(0));
		assertTrue(UMLHelper.isStereotypeApplied(this.model, "secure dependency"));
		assertNotNull(UMLsecUtil.getStereotypeApplication(class2, UMLsec.CRITICAL));
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
		SecureDependencyViolation vio = secureDependencyViolations.get(0);
		assertEquals(dependency1, vio.getDependency());
		assertEquals(class1, vio.getClient());
		assertEquals(class2, vio.getSupplier());
	}
	
	@Test
	public final void testSecureDependency3() {
		this.model = loadModel(this.filepath, "testSecureDependency3.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	
	@Test
	public final void testSecureDependency4() {
		this.model = loadModel(this.filepath, "testSecureDependency4.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void testSecureDependency5() {
		this.model = loadModel(this.filepath, "testSecureDependency5.uml");
		Package pkg = checkedGetElement(this.model, "testSecureDependency5", Package.class);
		Class class1 = checkedGetElement(pkg, "Class1", Class.class);
		Interface interface1 = checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = checkedGetElement(pkg, "Dep1", Dependency.class);
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
		SecureDependencyViolation vio = secureDependencyViolations.get(0);
		assertEquals(dependency1, vio.getDependency());
		assertEquals(class1, vio.getClient());
//		assertEquals(interface1, vio.getSupplier());
	}

	@Test
	public final void testSecureDependency6() {
		this.model = loadModel(this.filepath, "testSecureDependency6.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void testDepOnlyBspBuch() {
		this.model = loadModel(this.filepath, "SecureDependencyDependencyOnlyBspBuch.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Dependency callDep = checkedGetElement(this.model, "Dependency2", Dependency.class);
		assertTrue(UMLsecUtil.hasStereotype(callDep, UMLsec.CALL));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}

	@Test
	public final void testUsageOnlyBspBuch() {
		this.model = loadModel(this.filepath, "SecureDependencyUsageOnlyBspBuch.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = checkedGetElement(this.model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	
	@Test
	public final void testGeneralizedCritical() {
		this.model = loadModel(this.filepath, "testGeneralizedCritical.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = checkedGetElement(this.model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		//Assumption : See model
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void testSpecializedCritical() {
		this.model = loadModel(this.filepath, "testSpecializedCritical.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = checkedGetElement(this.model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		//Assumption : See model
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void test2InterfacesDifferentMethods() {
		this.model = loadModel(this.filepath, "2InterfacesDifferentMethods.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = checkedGetElement(this.model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		//fail("Noch keine Bedingung definiert!");
		assertEquals(2, secureDependencyViolations.size());
	}

	@Test
	public final void test2InterfacesSameMethods() {
		this.model = loadModel(this.filepath, "2InterfacesSameMethod.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = checkedGetElement(this.model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(this.model);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}

	private Model loadModel(String filepath, String name) {
		File file = new File(new File(filepath), name);
		if(file.exists()){
			Resource r = new ResourceSetImpl().createResource(URI.createURI(file.getAbsolutePath()));
			try(FileInputStream in = new FileInputStream(file)){
				r.load(in, Collections.EMPTY_MAP);
				EList<EObject> contents = r.getContents();
				assertTrue(1 <= contents.size());
				EObject obj = contents.get(0);
				assertTrue(obj instanceof Model);
				return (Model) obj;
			} catch (IOException e) {
				Logger.log(LogLevel.ERROR, "", e);
				fail();
			}
		}
		fail();
		return null;
	}
	
	/**
	 * Returns an adequately qualified element of a given type.
	 * Unit testing fails if element not found
	 * @param pkg
	 * @param adequatelyQualifiedName
	 * @param type
	 * @return - the element you asked for
	 */
	public static <T extends NamedElement> T checkedGetElement(final Package pkg, final String adequatelyQualifiedName, final java.lang.Class<T> type) {
		T targetElement = null;
		try {
			targetElement = UMLHelper.getElementOfNameAndType(pkg, adequatelyQualifiedName, type);
		} catch (ModelElementNotFoundException e) {
			fail(e.getMessage());
		}
		assertNotNull(targetElement);
		return targetElement;
	}

}
