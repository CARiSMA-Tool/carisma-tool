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
package carisma.check.smartcard.evolution.authorizedstatuscheck;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.uml2.uml.Constraint;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.OpaqueExpression;
import org.eclipse.uml2.uml.Region;
import org.eclipse.uml2.uml.State;
import org.eclipse.uml2.uml.Transition;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.analysis.AnalysisHost;
import carisma.core.analysis.RegisterInUseException;
import carisma.core.analysis.RegisterNotInUseException;
import carisma.core.analysis.UserAbortedAnalysisException;
import carisma.core.analysis.result.AnalysisResultMessage;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.DeltaList;
import carisma.evolution.SubstElement;
import carisma.evolution.uml2.ModifierMap;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;
import carisma.tests.modelutils.uml.TestHelper;


public class AuthorizedStatusEvolutionChecksTest {
	
	private class TestHost implements AnalysisHost{
		
		public TestHost() {
		}


		@Override
		public Resource getAnalyzedModel() {
			if (AuthorizedStatusEvolutionChecksTest.this.model != null) {
				return AuthorizedStatusEvolutionChecksTest.this.model.eResource();
			}
			return null;
		}
		
		
		@Override
		public Object getFromRegister(String registerName) {
			if (registerName.equals(AuthorizedStatusEvolutionModifierCheck.PRECONDITION_MODIFIERS_REGISTRY_KEY)) {
				return AuthorizedStatusEvolutionChecksTest.this.modifierMap;
			}
			return new DeltaList(AuthorizedStatusEvolutionChecksTest.this.deltas);
		}

		@Override
		public void addResultMessage(AnalysisResultMessage detail) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void appendToReport(String text) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public void appendLineToReport(String text) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public String getCurrentModelFilename() {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public void putToRegister(String registerName, Object data)
				throws RegisterInUseException {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean isRegisterInUse(String registerName) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public Object removeFromRegister(String registerName)
				throws RegisterNotInUseException {
			// TODO Auto-generated method stub
			return null;
		}


		@Override
		public void displayError(String message) {
			// TODO Auto-generated method stub
			Logger.log(LogLevel.INFO, message);
		}


		@Override
		public File getFileToBeWritten(File file)
				throws UserAbortedAnalysisException {
			// TODO Auto-generated method stub
			return file;
		}
		
	}
	
	/**
	 * Constant String for the name of the Stereotype 'authorized-status'.
	 */
	private static final String AUTHORIZED_STATUS = "authorized-status";
	
	/**
     * Constant String for the qualified name of the Stereotype 'authorized-status'.
     */
    private static final String QUALIFIED_AUTHORIZED_STATUS = "UMLsec::authorized-status";
    
    /**
     * Constant String for the name of the TaggedValue 'permission'.
     */
    private static final String PERMISSION =  "permission";
    
    /**
     * Constant String for the name of the key 'name'.
     */
    private static final String NAME = "name";
	
	private AnalysisHost testHost = null;
	
	private String filepath = "resources/models/";
	
	Model model = null;
	
	ModifierMap modifierMap = null;

	List<Delta> deltas = null;
		
	public final void init(final String modelfilename) {
		this.testHost = new TestHost();
		this.deltas = new ArrayList<>();
		this.model = TestHelper.loadModel(this.filepath, modelfilename);
		this.modifierMap = new ModifierMap(this.model.eResource());
		assertNotNull(this.modifierMap);
	}
	
	@After
	public final void cleanUp() {
		if (this.model != null) {
			TestHelper.unloadModel(this.model);
			this.model = null;
		}
	}
	
	
	@Test
	public final void testASDeleteGuard() {
		init("testASDeleteGuard.uml");
		try {
			AuthorizedStatusEvolutionModifierCheck theCheck = new AuthorizedStatusEvolutionModifierCheck();
			Transition el = (Transition) UMLHelper.getElementByName(this.model, "Uebergang");
			assertNotNull(el);
			Element guard = el.getGuard();
			assertNotNull(guard);
			DelElement delGuard = new DelElement(guard);
			List<DeltaElement> content = new ArrayList<>();
			content.add(delGuard);
			this.deltas.add(new Delta(new ArrayList<String>(), content));
			assertFalse(theCheck.perform(null, this.testHost));
			State authorizedState = UMLHelper.getElementOfNameAndType(this.model, "State1", State.class);
			assertNotNull(authorizedState);
			assertTrue(UMLHelper.isStereotypeApplied(authorizedState, AUTHORIZED_STATUS));
			assertTrue(UMLHelper.unapplyStereotype(authorizedState, QUALIFIED_AUTHORIZED_STATUS));
			assertFalse(UMLHelper.isStereotypeApplied(authorizedState, AUTHORIZED_STATUS));
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), content));
			assertTrue(theCheck.perform(null, this.testHost));
			assertNotNull(UMLHelper.applyStereotype(authorizedState, QUALIFIED_AUTHORIZED_STATUS));
			StereotypeApplication stapp = UMLHelper.getStereotypeApplication(authorizedState, AUTHORIZED_STATUS);
			assertNotNull(stapp);
			stapp.getTaggedValue(PERMISSION).setValue("");
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), content));
			assertTrue(theCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
			fail(e.getMessage());
		}
		
	}
	
	@Test
	public final void testASAddTransition() {
		init("testASAddTransition.uml");
		try {
			AuthorizedStatusEvolutionModifierCheck theCheck = new AuthorizedStatusEvolutionModifierCheck();
			Region region1 = (Region) UMLHelper.getElementByName(this.model, "Region");
			assertNotNull(region1);
			Transition transition = (Transition) UMLHelper.getElementByName(this.model, "Transition1");
			AddElement addTransition = new AddElement(region1, transition.eClass(), null);
			addTransition.addKeyValuePair(NAME, "newTransition");
			addTransition.addKeyValuePair("source", "State2");
			addTransition.addKeyValuePair("target", "State1");
			List<DeltaElement> content = new ArrayList<>();
			content.add(addTransition);
			this.deltas.add(new Delta(new ArrayList<String>(), content));
			assertFalse(theCheck.perform(null, this.testHost));
			Region targetRegion = UMLHelper.getElementOfNameAndType(this.model, "Region", Region.class);
			assertNotNull(targetRegion);
			AddElement addGuard = new AddElement(null, UMLHelper.getMetaClass("Constraint"), addTransition);
			addGuard.addKeyValuePair(NAME, "newGuard");
			addGuard.addKeyValuePair("specification", "Dudel");
			addTransition.addContainedElement(addGuard);
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), content));
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertTrue(theCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail("Couldn't find a model element.");
		} catch (InvalidMetaclassException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail("Test syntax error.");			
		}
	}
	
	@Test
	public final void testASSubstPermission() {
		init("testASSubstPermission.uml");
		try {
			State authorizedState = UMLHelper.getElementOfNameAndType(this.model, "State1", State.class);
			assertNotNull(authorizedState);
			StereotypeApplication authorizedApp = UMLHelper.getStereotypeApplication(authorizedState, AUTHORIZED_STATUS);
			assertNotNull(authorizedApp);
			TaggedValue permissionTag = authorizedApp.getTaggedValue(PERMISSION);
			assertNotNull(permissionTag);
			
			AddElement addPermission = new AddElement(authorizedApp, UMLHelper.getMetaClass("Property"), null);
			addPermission.addKeyValuePair(NAME, PERMISSION);
			addPermission.addKeyValuePair("value", "anders");		
			ArrayList<AddElement> substitutes = new ArrayList<>();
			substitutes.add(addPermission);
			SubstElement substPermission = new SubstElement(permissionTag, substitutes);
			
			List<DeltaElement> deltaContent = new ArrayList<>();
			deltaContent.add(substPermission);
			this.deltas.add(new Delta(new ArrayList<String>(), deltaContent));
			
			AuthorizedStatusEvolutionModifierCheck theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertFalse(theCheck.perform(null, this.testHost));
			Transition incomingTransition = UMLHelper.getElementOfNameAndType(this.model, "Uebergang", Transition.class);
			assertNotNull(incomingTransition);
			OpaqueExpression guardConstraint = (OpaqueExpression) incomingTransition.getGuard().getSpecification();
			assertNotNull(guardConstraint);
			guardConstraint.getBodies().remove(0);
			guardConstraint.getBodies().add("anders");
			
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), deltaContent));
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertTrue(theCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail("Couldn't find a model element.");
		} catch (InvalidMetaclassException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail("Test syntax error.");			
		}
	}
	
	@Test
	public final void testASSubstGuard() {
		init("testASSubstGuard.uml");
		try {
			Transition ueber = UMLHelper.getElementOfNameAndType(this.model, "Uebergang", Transition.class);
			assertNotNull(ueber);
			Constraint guard = ueber.getGuard();
			assertNotNull(guard);
			
			AddElement addGuard = new AddElement(ueber, guard.eClass(), null);
			addGuard.addKeyValuePair(NAME, "newConstraint");
			addGuard.addKeyValuePair("specification", "anders");
			addGuard.addKeyValuePair("language", "Analysis");
			
			ArrayList<AddElement> substitutes = new ArrayList<>();
			substitutes.add(addGuard);
			SubstElement substGuard = new SubstElement(guard, substitutes);
			
			List<DeltaElement> deltaContent = new ArrayList<>();
			deltaContent.add(substGuard);
			
			this.deltas.add(new Delta(new ArrayList<String>(), deltaContent));
			AuthorizedStatusEvolutionModifierCheck theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertFalse(theCheck.perform(null, this.testHost));
			
			State authorizedState = UMLHelper.getElementOfNameAndType(this.model, "State1", State.class);
			assertNotNull(authorizedState);
			UMLHelper.unapplyStereotype(authorizedState, QUALIFIED_AUTHORIZED_STATUS);
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), deltaContent));
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertTrue(theCheck.perform(null, this.testHost));
			
			UMLHelper.applyStereotype(authorizedState, QUALIFIED_AUTHORIZED_STATUS);
			StereotypeApplication stapp = UMLsecUtil.getStereotypeApplication(authorizedState, UMLsec.AUTHORIZED_STATUS);
			assertNotNull(stapp);
			stapp.getTaggedValue(PERMISSION).removeValue();
			
			this.deltas.clear();
			this.deltas.add(new Delta(new ArrayList<String>(), deltaContent));
			theCheck = new AuthorizedStatusEvolutionModifierCheck();
			assertTrue(theCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
            Logger.log(LogLevel.ERROR, e.getMessage(), e);
			fail("Couldn't find a model element.");
		}
	}
}
