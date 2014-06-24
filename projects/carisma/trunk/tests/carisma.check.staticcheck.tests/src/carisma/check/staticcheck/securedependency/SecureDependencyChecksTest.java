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

import static org.junit.Assert.*;

import java.util.List;

import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Interface;
import org.eclipse.uml2.uml.InterfaceRealization;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Usage;
import org.junit.After;
import org.junit.Test;

import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;
import carisma.tests.modelutils.uml.TestHelper;


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
		TestHelper.unloadModel(model);
		model = null;
	}
	
	/**
	 * This tests whether 
	 */
	@Test
	public final void testSecureDependency1() {
		model = TestHelper.loadModel(filepath, "testSecureDependency1.uml");
		Package pkg = TestHelper.checkedGetElement(model, "testSecureDependency1", Package.class);
		Class class1 = TestHelper.checkedGetElement(pkg, "Class1", Class.class);
		Class class2 = TestHelper.checkedGetElement(pkg, "Class2", Class.class);
		Interface interface1 = TestHelper.checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = TestHelper.checkedGetElement(pkg, "Dep1", Dependency.class);
		InterfaceRealization realization1 = TestHelper.checkedGetElement(pkg, "InterfaceRealization1", InterfaceRealization.class);
		
		assertEquals(UMLHelper.getStereotypeApplications(dependency1).toString(),1, UMLHelper.getStereotypeApplications(dependency1).size());
		assertTrue(UMLHelper.isStereotypeApplied(dependency1, "call"));
		assertEquals(class1, dependency1.getClients().get(0));
		assertEquals(interface1, dependency1.getSuppliers().get(0));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_DEPENDENCY));
		assertNotNull(UMLsecUtil.getStereotypeApplication(class2, UMLsec.CRITICAL));
		assertEquals(interface1, realization1.getSuppliers().get(0));
		assertEquals(class2, realization1.getClients().get(0));
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	
	@Test
	public final void testSecureDependency2() {
		model = TestHelper.loadModel(filepath, "testSecureDependency2.uml");
		Package pkg = TestHelper.checkedGetElement(model, "testSecureDependency2", Package.class);
		Class class1 = TestHelper.checkedGetElement(pkg, "Class1", Class.class);
		Class class2 = TestHelper.checkedGetElement(pkg, "Class2", Class.class);
		Interface interface1 = TestHelper.checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = TestHelper.checkedGetElement(pkg, "Dep1", Dependency.class);
		
		assertEquals(UMLHelper.getStereotypeApplications(dependency1).toString(), 2, UMLHelper.getStereotypeApplications(dependency1).size());
		assertTrue(UMLHelper.isStereotypeApplied(dependency1, "call"));
		assertEquals(class1, dependency1.getClients().get(0));
		assertEquals(interface1, dependency1.getSuppliers().get(0));
		assertTrue(UMLHelper.isStereotypeApplied(model, "secure dependency"));
		assertNotNull(UMLsecUtil.getStereotypeApplication(class2, UMLsec.CRITICAL));
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
		SecureDependencyViolation vio = secureDependencyViolations.get(0);
		assertEquals(dependency1, vio.getDependency());
		assertEquals(class1, vio.getClient());
		assertEquals(interface1, vio.getSupplier());
	}
	
	@Test
	public final void testSecureDependency3() {
		model = TestHelper.loadModel(filepath, "testSecureDependency3.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}
	
	@Test
	public final void testSecureDependency4() {
		model = TestHelper.loadModel(filepath, "testSecureDependency4.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void testSecureDependency5() {
		model = TestHelper.loadModel(filepath, "testSecureDependency5.uml");
		Package pkg = TestHelper.checkedGetElement(model, "testSecureDependency5", Package.class);
		Class class1 = TestHelper.checkedGetElement(pkg, "Class1", Class.class);
		Interface interface1 = TestHelper.checkedGetElement(pkg, "Interface1", Interface.class);
		Dependency dependency1 = TestHelper.checkedGetElement(pkg, "Dep1", Dependency.class);
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(1, secureDependencyViolations.size());
		SecureDependencyViolation vio = secureDependencyViolations.get(0);
		assertEquals(dependency1, vio.getDependency());
		assertEquals(class1, vio.getClient());
		assertEquals(interface1, vio.getSupplier());
	}

	@Test
	public final void testSecureDependency6() {
		model = TestHelper.loadModel(filepath, "testSecureDependency6.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
	}

	@Test
	public final void testDepOnlyBspBuch() {
		model = TestHelper.loadModel(filepath, "SecureDependencyDependencyOnlyBspBuch.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Dependency callDep = TestHelper.checkedGetElement(model, "Dependency2", Dependency.class);
		assertTrue(UMLsecUtil.hasStereotype(callDep, UMLsec.CALL));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(0, secureDependencyViolations.size());
		sdc.checkSecureDependency(model, false);
		secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}

	@Test
	public final void testUsageOnlyBspBuch() {
		model = TestHelper.loadModel(filepath, "SecureDependencyUsageOnlyBspBuch.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = TestHelper.checkedGetElement(model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
		sdc.checkSecureDependency(model, false);
		secureDependencyViolations = sdc.getViolations();
		assertEquals(2, secureDependencyViolations.size());
	}
	
	@Test
	public final void testGeneralizedCritical() {
		model = TestHelper.loadModel(filepath, "generalizedCritical.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = TestHelper.checkedGetElement(model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		fail("Noch keine Bedingung definiert!");
	}

	@Test
	public final void testSpecializedCritical() {
		model = TestHelper.loadModel(filepath, "spezializedCritical.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = TestHelper.checkedGetElement(model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		fail("Noch keine Bedingung definiert!");
	}

	@Test
	public final void test2InterfacesDifferentMethods() {
		model = TestHelper.loadModel(filepath, "2InterfacesDifferentMethods.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = TestHelper.checkedGetElement(model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		fail("Noch keine Bedingung definiert!");
	}

	@Test
	public final void test2InterfacesSameMethods() {
		model = TestHelper.loadModel(filepath, "2InterfacesSameMethod.uml");
		SecureDependencyChecks sdc = new SecureDependencyChecks(null);
		Usage callDep = TestHelper.checkedGetElement(model, "Usage1", Usage.class);
		assertTrue(UMLHelper.isStereotypeApplied(callDep, "call"));
		assertFalse(UMLsecUtil.hasStereotype(callDep, UMLsec.HIGH));
		
		sdc.checkSecureDependency(model, true);
		List<SecureDependencyViolation> secureDependencyViolations = sdc.getViolations();
		assertEquals(4, secureDependencyViolations.size());
	}

}