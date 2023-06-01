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
package carisma.tool.evolution.uml2.umlchange;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.List;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.evolution.Alternative;
import carisma.evolution.Change;
import carisma.evolution.DelElement;
import carisma.evolution.DeltaElement;
import carisma.evolution.EditElement;
import carisma.evolution.uml2.umlchange.UMLchangeParser;
import carisma.modeltype.uml2.UMLHelper;
import carisma.profile.umlchange.UMLchange;
import carisma.tests.modelutils.uml.TestHelper;


/**
 * 
 * @author Berghoff
 *
 */
public class BBKeepTest {

	/**
	 * the path of the directory where the testmodels lay in.
	 */
	private String testmodeldir = "resources/models/subst/keep";
	
	/**
	 * Constant variable for Attribute owner of an EditElement.
	 */
	private static final String OWNER = "owner";
	
	/**
	 * the original model.
	 */
	private Model model = null;

	/**
	 * UMLchangeParser.
	 */
	private UMLchangeParser parser = null;
	
	/** 
	 * <<keep>> on association leads to ignoring the <<keep>>.
	 */
	@Test
	public final void keepAssoziationTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "KeepAssociation.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		assertEquals(2, changes.get(0).getAlternatives().get(0).getDeltaElements().size());

		assertTrue(changes.get(0).getAlternatives().get(0).getDeltaElements().get(0) instanceof DelElement);
		assertTrue(changes.get(0).getAlternatives().get(0).getDeltaElements().get(1) instanceof EditElement);
		
		
		EditElement edit = (EditElement) changes.get(0).getAlternatives().get(0).getDeltaElements().get(1);

		assertEquals(1, edit.getValues().size());
		assertEquals("Keep", ((NamedElement) edit.getValues().get(OWNER)).getName());
		
	}
	
	/** 
	 * Input is a Model with one <<subst>> with one alternative referencing a Namespace.
	 * And one <<keep>> corresponding to the <<subst>> alternative. 
	 */
	@Test
	@Ignore
	public final void complexKeepTest() { 
		this.model = TestHelper.loadModel(this.testmodeldir, "ComplexAdopterParserKeep.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		
		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		
		List<DeltaElement> deltaEles = alter1.getDeltaElements();

		assertEquals(4, deltaEles.size());

		assertTrue(deltaEles.get(1) instanceof DelElement);
		
		assertTrue(deltaEles.get(0) instanceof EditElement);
		assertTrue(deltaEles.get(2) instanceof EditElement);
		assertTrue(deltaEles.get(3) instanceof EditElement);
		
		boolean hastoBeKeptTarget = false; 
		
		for (DeltaElement deltaEle : deltaEles) { 
			if (deltaEle.getTarget() == TestHelper.checkedGetElement(this.model, "toBeKept", Operation.class)) {
				hastoBeKeptTarget = true;
				break;
			}
		}
		assertTrue(hastoBeKeptTarget);
		
		for (DeltaElement deltaEle : deltaEles) { 
			if (deltaEle instanceof EditElement) {
				EditElement edEle = (EditElement) deltaEle;
				
				if (((NamedElement) deltaEle.getTarget()) == TestHelper.checkedGetElement(this.model, "NewClass", Class.class)) { 
					assertEquals(TestHelper.checkedGetElement(this.model, "model::ComplexAdopterParserKeep", Package.class), edEle.getValues().get(OWNER));
					
				} else if (((NamedElement) deltaEle.getTarget()) == TestHelper.checkedGetElement(this.model, "MyNamespace::OldClass", Class.class)) {
					assertEquals(TestHelper.checkedGetElement(this.model, "model::ComplexAdopterParserKeep", Package.class), edEle.getValues().get(OWNER));
					
				} else if (((NamedElement) deltaEle.getTarget()) == TestHelper.checkedGetElement(this.model, "toBeKept", Operation.class)) {
					assertEquals("OldClass", edEle.getValues().get("name"));
					assertEquals(TestHelper.checkedGetElement(this.model, "MyNamespace::OldClass", Class.class), edEle.getValues().get(OWNER));
				} else { 
					fail(edEle.toString());
				}
			}
		}
	}
	
	/** 
	 * <<subst>> on a package, the class 'OldClass' in the this package has an operation with <<keep>>.
	 * But the Package MyNamespace doesn't have a class 'OldClass'.
	 */
	@Test
	@Ignore
	public final void missingAdopterTest() { 
		this.model = TestHelper.loadModel(this.testmodeldir, "WrongNamespace.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		
		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		
		List<DeltaElement> deltaEles = alter1.getDeltaElements();

		assertEquals(2, deltaEles.size());

	
		assertEquals(TestHelper.checkedGetElement(this.model, "NewClass", Class.class), deltaEles.get(1).getTarget());
		assertEquals(
				TestHelper.checkedGetElement(this.model, "model::ComplexAdopterParserKeep", Package.class), 
				((EditElement) deltaEles.get(1)).getValues().get(OWNER));
		
	}
	
	/**
	 * Keep on second Alternative of <<subst>>.
	 */
	@Test
	@Ignore
	public final void keepForSecondAlternativeTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "KeepForSecondAlternative.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(2, changes.get(0).getAlternatives().size());
		
		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		assertEquals(1, alter1.getDeltaElements().size());
		
		Alternative alter2 = changes.get(0).getAlternatives().get(1);
		
		assertEquals(3, alter2.getDeltaElements().size());
		
		boolean hasEditElement = false;
		for (DeltaElement deltaEle : alter2.getDeltaElements()) {
			if (TestHelper.checkedGetElement(this.model, "NewClass", Class.class) == deltaEle.getTarget()) {
				assertEquals(TestHelper.checkedGetElement(this.model, "KeepForSecondAlternative", Package.class), ((EditElement) deltaEle).getValues().get(OWNER));
				hasEditElement = true;
				break;
			}
		}
		assertTrue("EditElement with correct Target notFound", hasEditElement);
	}
	
	/** 
	 * Keep on Class with Operation. Assert that no DelElement for the Operation is created.
	 * Plus whether the expected EditElements are created correctly.
	 */
	@Test
	@Ignore
	public final void classWithOperationTest() {
		this.model = TestHelper.loadModel(this.testmodeldir, "KeepOnEleWithChildren.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());
		
		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		assertEquals(3, alter1.getDeltaElements().size());
		
		DelElement del = (DelElement) alter1.getDeltaElements().get(1);
		
		for (EObject deletion : del.getAccompanyingDeletions()) {
			if (TestHelper.checkedGetElement(this.model, "Operation1", Operation.class) == deletion
					|| TestHelper.checkedGetElement(this.model, "OldClass", Class.class) == deletion) {
				fail("Created wrong Deletion");
			}
		}
		
		
		boolean hasEditPackageOwner = false; 
		for (DeltaElement ele : alter1.getDeltaElements()) {
			if (ele.getTarget() == TestHelper.checkedGetElement(this.model, "InnerPackage", Package.class)) {
				hasEditPackageOwner = true;
				assertEquals(TestHelper.checkedGetElement(this.model, "KeepOnEleWithChildren", Package.class), ((EditElement) ele).getValues().get(OWNER));
			}
		}
		assertTrue("Does not have EditElement to change Change Owner of the Package", hasEditPackageOwner);
		
		boolean hasEditClassOwner = false; 
		for (DeltaElement ele : alter1.getDeltaElements()) {
			if (ele.getTarget() == TestHelper.checkedGetElement(this.model, "OldClass", Class.class)) {
				hasEditClassOwner = true;
				assertEquals(TestHelper.checkedGetElement(this.model, "InnerPackage", Package.class), ((EditElement) ele).getValues().get(OWNER));
			}
		}
		assertTrue("Does not have EditElement to change Change Owner of the Class", hasEditClassOwner);
	}
	
	/** 
	 * Test if <<keep>> for SimpleElementDescription in <<subst>> new is ignored.
	 */
	@Test
	@Ignore
	public final void adoptorSimpleEleDescription() {
		this.model = TestHelper.loadModel(this.testmodeldir, "AdopterSimpleEleDescription.uml");
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLchange.DESCRIPTOR));
		this.parser = new UMLchangeParser(this.model);
		assertNotNull(this.parser); 
		List<Change> changes = this.parser.generateDeltaDescriptions();
		assertEquals(1, changes.size());
		assertEquals(1, changes.get(0).getAlternatives().size());

		Alternative alter1 = changes.get(0).getAlternatives().get(0);
		assertEquals(1, alter1.getDeltaElements().size());
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
