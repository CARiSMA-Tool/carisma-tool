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
package carisma.check.staticcheck.evolution.securedependency;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Classifier;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Interface;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.UMLPackage;
import org.eclipse.uml2.uml.Usage;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.check.staticcheck.securedependency.SecureDependencyViolation;
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
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;

/**
 * To test the implementation of the secure usageDependency evolution check.
 * @author Sven Wenzel
 *
 */
public class SecureDependencyEvolutionCheckTest {
	
	private class TestHost implements AnalysisHost {

		public TestHost() {
		}

		@Override
		public void addResultMessage(final AnalysisResultMessage detail) {
			Logger.log(LogLevel.INFO, detail.getText());
			
		}

		@Override
		public void appendToReport(final String text) {
		    Logger.log(LogLevel.INFO, text);			
		}

		@Override
		public void appendLineToReport(final String text) {
		    Logger.log(LogLevel.INFO, text);			
		}

		@Override
		public Resource getAnalyzedModel() {
			return SecureDependencyEvolutionCheckTest.this.modelres;
		}

		@Override
		public String getCurrentModelFilename() {
			return SecureDependencyEvolutionCheckTest.this.modelres.getURI().toFileString();
		}

		@Override
		public void putToRegister(final String registerName, final Object data)
				throws RegisterInUseException {
			// TODO Auto-generated method stub
		}

		@Override
		public boolean isRegisterInUse(final String registerName) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public Object getFromRegister(final String registerName)
				throws RegisterNotInUseException {
			if (registerName.equals(SecureDependencyEvolutionCheck.PRECONDITION_DELTAS_REGISTER_KEY)) {
				return new DeltaList(SecureDependencyEvolutionCheckTest.this.deltas);
			} else if (registerName.equals(SecureDependencyEvolutionCheck.PRECONDITIONS_MODIFIERS_REGISTRY_KEY)) {
				return SecureDependencyEvolutionCheckTest.this.modifierMap;
			}
			return null;
		}

		@Override
		public Object removeFromRegister(final String registerName)
				throws RegisterNotInUseException {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public void displayError(final String message) {
			// TODO Auto-generated method stub
		    Logger.log(LogLevel.INFO, message);
		}

		@Override
		public File getFileToBeWritten(final File file)
				throws UserAbortedAnalysisException {
			// TODO Auto-generated method stub
			return file;
		}
	}
	
	/**
	 * Constant name for the key 'name'.
	 */
	private static final String NAME = "name";
	
	/**
	 * Constant name for the key 'value'.
	 */
	private static final String VALUE = "value";
	
	/**
	 * Constant name for the key 'client'.
	 */
	private static final String CLIENT = "client";
	
	/**
	 * Constant name for the key 'supplier'.
	 */
	private static final String SUPPLIER = "supplier";
	
	/**
	 * Constant name for the value 'Standard::Call'.
	 */
	private static final String STANDARD_CALL = "Standard::Call";
	
	/**
	 * Constant name for the value 'Usage34'.
	 */
	
	private static final String USAGE34 = "Usage34";
	
	/**
	 * Constant name for the value 'integrity'.
	 */
	private static final String INTEGRITY = "integrity";
	
	/**
	 * Constant name for the value 'op2a()'.
	 */
	private static final String OP2A = "op2a()";
	
	/**
	 * Constant name for the value 'op5()'.
	 */
	private static final String OP5 = "op5()";
	
	/**
	 * Constant name for TaggedValue 'high'.
	 */
	private static final String HIGH = "high";
	
	/**
	 * Constant name for Stereotype 'UMLsec::critical'.
	 */
	private static final String UMLSEC_CRITICAL = "UMLsec::critical";
	
	/**
	 * Simple Constant name for Stereotype 'critical'.
	 */
	private static final String CRITICAL = "critical";
	
	/**
	 * Constant String for the name of of an Element.
	 */
	private static final String CLIENT_3 = "Client3";
	
	/**
     * Constant String for the name of of an Element.
     */
    private static final String SUPPLIER_4 = "Supplier4";
	
	
	private AnalysisHost testHost = null;
	
	private String filepath = "resources/models/securedependency";
	
	private ResourceSet rs = new ResourceSetImpl();
	
	Resource modelres = null;
	
	private Package model = null;
	
	List<Delta> deltas = null;
	
	ModifierMap modifierMap = null;
	
	private Classifier client1 = null;
	private Classifier client2 = null;
	private Classifier client3 = null;
	private Classifier client5 = null;
	private Classifier supplier1 = null;
	private Classifier supplier2 = null;
	private Classifier supplier5 = null;
	private Interface isupplier = null;
	private Dependency dep1 = null;
	private Dependency dep2 = null;
	private Dependency dep5 = null;
	
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	public final static <T extends NamedElement> T getMember(Package model, String name, Class<T> clazz) {
		T ne = null;
		try {
			ne = UMLHelper.getElementOfNameAndType(model, name, clazz); 
					model.getMember(name);
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
		assertNotNull(ne);
		return ne;
	}
	
	public final void initializeSDEBasisModel() {
		this.client1 = getMember(this.model, "Client1", Classifier.class);
		this.client2 = getMember(this.model, "Client2", Classifier.class);
		this.client3 = getMember(this.model, CLIENT_3, Classifier.class);
		this.client5 = getMember(this.model, "Client5", Classifier.class);
		this.supplier1 = getMember(this.model, "Supplier1", Classifier.class);
		this.supplier2 = getMember(this.model, "Supplier2", Classifier.class);
		this.supplier5 = getMember(this.model, "Supplier5", Classifier.class);
		this.isupplier = getMember(this.model, "ISupplier", Interface.class);
		this.dep1 = getMember(this.model, "Dep1", Dependency.class);
		this.dep2 = getMember(this.model, "Dep2", Dependency.class);
		this.dep5 = getMember(this.model, "Dep5", Dependency.class);
		assertTrue(UMLHelper.isProfileApplied(this.model, UMLsec.DESCRIPTOR));
	}
	
	public final void init(final String modelfilename) {
		this.testHost = new TestHost();
		this.deltas = new ArrayList<>();
		boolean isBasisModel = false;
		if (modelfilename != null && (!modelfilename.isEmpty())) {
			try {
				loadModel(modelfilename);
			} catch (IOException e) {
				Logger.log(LogLevel.ERROR, "", e);
			}
		} else {
			try {
				loadModel("SDE-basis.uml");
			} catch (IOException e) {
				Logger.log(LogLevel.ERROR, "", e);
			}
			isBasisModel = true;
		}
		assertNotNull(this.modelres);
		this.modifierMap = new ModifierMap(this.modelres);
		assertNotNull(this.modifierMap);
		this.model = (Package) this.modelres.getContents().get(0);
		assertNotNull(this.model);
		if (isBasisModel) {
			initializeSDEBasisModel();
		}
	}
	
	@After
	public final void cleanUp() {
		this.model = null;
		this.modelres.unload();
	}
	
	@Test
	public final void testAddDependency3() {
		init(null);
		AddElement addUsage3 = new AddElement(this.model, UMLPackage.eINSTANCE.getUsage(), null);
		addUsage3.addKeyValuePair(NAME, "Usage3");
		addUsage3.addKeyValuePair(CLIENT, CLIENT_3);
		addUsage3.addKeyValuePair(SUPPLIER, "Supplier3");
		
		AddElement addCall = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage3);
		addCall.addKeyValuePair(NAME, STANDARD_CALL);
		addUsage3.addContainedElement(addCall);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addUsage3);
		this.deltas.add(new Delta(new ArrayList<String>(), elements));

		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}

	@Test
	@Ignore
	public final void testAddDependency34() {
		init(null);
		try {
			AddElement addUsage34 = new AddElement(this.model, UMLPackage.eINSTANCE.getUsage(), null);
			addUsage34.addKeyValuePair(NAME, USAGE34);
			addUsage34.addKeyValuePair(CLIENT, CLIENT_3);
			addUsage34.addKeyValuePair(SUPPLIER, SUPPLIER_4);
			
			AddElement addCall = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage34);
			addCall.addKeyValuePair(NAME, STANDARD_CALL);
			addUsage34.addContainedElement(addCall);
			
			List<DeltaElement> elements = new ArrayList<>();
			elements.add(addUsage34);
			this.deltas.add(new Delta(new ArrayList<String>(), elements));
			
			SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
			Usage usage34 = UMLHelper.getElementOfNameAndType(this.modifierMap.get(this.deltas.get(0)).getModifiedModel(),USAGE34,Usage.class);
			assertNotNull(usage34);
			assertEquals(1, usage34.getAppliedStereotypes().size());
			assertFalse(sdec.perform(null, this.testHost));
			assertEquals(2, sdec.getViolations().size());
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}

	@Test
	@Ignore
	public final void testAddDependency34Integrity() {
		init(null);
		AddElement addUsage34 = new AddElement(this.model, UMLPackage.eINSTANCE.getUsage(), null);
		addUsage34.addKeyValuePair(NAME, USAGE34);
		addUsage34.addKeyValuePair(CLIENT, CLIENT_3);
		addUsage34.addKeyValuePair(SUPPLIER, SUPPLIER_4);
		
		AddElement addCall = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage34);
		addCall.addKeyValuePair(NAME, STANDARD_CALL);
		addUsage34.addContainedElement(addCall);
		
		AddElement addIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage34);
		addIntegrity.addKeyValuePair(NAME, "UMLsec::integrity");
		addUsage34.addContainedElement(addIntegrity);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addUsage34);
		this.deltas.add(new Delta(elements));

		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	public final void testAddDependency34IntegrityAndCritical() {
		init(null);
		AddElement addUsage34 = new AddElement(this.model, UMLPackage.eINSTANCE.getUsage(), null);
		addUsage34.addKeyValuePair(NAME, USAGE34);
		addUsage34.addKeyValuePair(CLIENT, CLIENT_3);
		addUsage34.addKeyValuePair(SUPPLIER, SUPPLIER_4);
		
		AddElement addCall = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage34);
		addCall.addKeyValuePair(NAME, STANDARD_CALL);
		addUsage34.addContainedElement(addCall);
		
		AddElement addIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), addUsage34);
		addIntegrity.addKeyValuePair(NAME, "UMLsec::integrity");
		addUsage34.addContainedElement(addIntegrity);
		
		AddElement addCritical = new AddElement(this.client3, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical);
		setIntegrity.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity.addKeyValuePair(VALUE, "op4()");
		addCritical.addContainedElement(setIntegrity);

		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addUsage34);
		elements.add(addCritical);
		this.deltas.add(new Delta(elements));

		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}

	@Test
	@Ignore
	public final void testAddCriticalSupplier5() {
		init(null);
		AddElement addCritical = new AddElement(this.supplier5, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical);
		setIntegrity.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity.addKeyValuePair(VALUE, OP5);
		addCritical.addContainedElement(setIntegrity);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addCritical);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(2, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testAddCriticalSupplier5AndClient() {
		init(null);
		AddElement addCritical = new AddElement(this.supplier5, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical);
		setIntegrity.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity.addKeyValuePair(VALUE, OP5);
		addCritical.addContainedElement(setIntegrity);
		
		AddElement addCritical2 = new AddElement(this.client5, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical2.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity2 = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical2);
		setIntegrity2.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity2.addKeyValuePair(VALUE, OP5);
		addCritical2.addContainedElement(setIntegrity2);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addCritical);
		elements.add(addCritical2);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	public final void testAddCriticalSupplier5AndClientAndDep() {
		init(null);
		AddElement addCritical = new AddElement(this.supplier5, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical);
		setIntegrity.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity.addKeyValuePair(VALUE, OP5);
		addCritical.addContainedElement(setIntegrity);
		
		AddElement addCritical2 = new AddElement(this.client5, UMLPackage.eINSTANCE.getStereotype(), null);
		addCritical2.addKeyValuePair(NAME, UMLSEC_CRITICAL);
		
		AddElement setIntegrity2 = new AddElement(null, UMLPackage.eINSTANCE.getProperty(), addCritical2);
		setIntegrity2.addKeyValuePair(NAME, INTEGRITY);
		setIntegrity2.addKeyValuePair(VALUE, OP5);
		addCritical2.addContainedElement(setIntegrity2);
		
		AddElement addIntegrity = new AddElement(this.dep5, UMLPackage.eINSTANCE.getStereotype(), null);
		addIntegrity.addKeyValuePair(NAME, "UMLsec::integrity");

		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addCritical);
		elements.add(addCritical2);
		elements.add(addIntegrity);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}
	
	@Test
	public final void testAddTagTrue() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		AddElement addIntegrityValue = new AddElement(criticalApp, UMLPackage.eINSTANCE.getProperty(), null);
		addIntegrityValue.addKeyValuePair(NAME, INTEGRITY);
		addIntegrityValue.addKeyValuePair(VALUE, OP2A);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addIntegrityValue);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}

	@Test
	@Ignore
	public final void testAddTagFalse() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		AddElement addIntegrityValue = new AddElement(criticalApp, UMLPackage.eINSTANCE.getProperty(), null);
		addIntegrityValue.addKeyValuePair(NAME, INTEGRITY);
		addIntegrityValue.addKeyValuePair(VALUE, "op2b()");
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addIntegrityValue);
		this.deltas.add(new Delta(elements));

		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(2, sdec.getViolations().size());
	}

	@Test
	public final void testRemoveDep1() {
		init(null);
		DelElement delDep1 = new DelElement(this.dep1);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delDep1);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}

	@Test
	@Ignore
	public final void testRemoveSupplier2() {
		init(null);
		DelElement delSupplier2 = new DelElement(this.supplier2);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delSupplier2);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	public final void testRemoveClient1() {
		init(null);
		DelElement delClient1 = new DelElement(this.client1);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delClient1);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
		Logger.log(LogLevel.DEBUG, String.valueOf(sdec.getViolations().size()));
		for (SecureDependencyViolation v : sdec.getViolations()) {
	        Logger.log(LogLevel.DEBUG, v.toString());
		}
	}
	
	@Test
	@Ignore
	public final void testRemoveSecrecy() {
		init(null);
		StereotypeApplication secrecyApp = UMLHelper.getStereotypeApplication(this.dep1, "secrecy");
		DelElement delApp = new DelElement(secrecyApp);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delApp);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}
	
	@Test
	@Ignore
	public final void testRemoveSecrecyTag() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier1, CRITICAL);
		TaggedValue secrecyTagValue = criticalApp.getTaggedValue("secrecy");
		DelElement delTagValue = new DelElement(secrecyTagValue);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delTagValue);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testRemoveSupplierCritical() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier1, CRITICAL);
		DelElement delCritical = new DelElement(criticalApp);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delCritical);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testRemoveClientCritical() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.client1, CRITICAL);
		DelElement delCritical = new DelElement(criticalApp);
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(delCritical);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testSubstDepSecrecy() {
		init(null);
		StereotypeApplication secrecyApp = UMLHelper.getStereotypeApplication(this.dep1, "secrecy");
		AddElement addHigh = new AddElement(this.dep1, UMLPackage.eINSTANCE.getStereotype(), null);
		addHigh.addKeyValuePair(NAME, "UMLsec::high");
		SubstElement substSecrecy = new SubstElement(secrecyApp, Arrays.asList(addHigh));
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(substSecrecy);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testSubstTagSecrecy() {
		init(null);
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		TaggedValue highTagValue = criticalApp.getTaggedValue(HIGH);
		
		AddElement addHighValue = new AddElement(this.supplier2, UMLPackage.eINSTANCE.getProperty(), null);
		addHighValue.addKeyValuePair(NAME, HIGH);
		addHighValue.addKeyValuePair(VALUE, "op2b()");
		SubstElement substHighValues = new SubstElement(highTagValue, Arrays.asList(addHighValue));
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(substHighValues);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	public final void testSubstNewOperation() {
		init(null);
		AddElement addOp2a = new AddElement(this.isupplier, UMLPackage.eINSTANCE.getOperation(), null);
		addOp2a.addKeyValuePair(NAME, OP2A);
		
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		TaggedValue highValue = criticalApp.getTaggedValue(HIGH);
		AddElement newHighValue = new AddElement(criticalApp, UMLPackage.eINSTANCE.getProperty(), null);
		newHighValue.addKeyValuePair(NAME, HIGH);
		newHighValue.addKeyValuePair(VALUE, OP2A);
		SubstElement substHighValues = new SubstElement(highValue, Arrays.asList(newHighValue));
		
		StereotypeApplication criticalApp2 = UMLHelper.getStereotypeApplication(this.client2, CRITICAL);
		TaggedValue highValue2 = criticalApp2.getTaggedValue(HIGH);
		AddElement newHighValue2 = new AddElement(criticalApp2, UMLPackage.eINSTANCE.getProperty(), null);
		newHighValue2.addKeyValuePair(NAME, HIGH);
		newHighValue2.addKeyValuePair(VALUE, OP2A);
		SubstElement substHighValues2 = new SubstElement(highValue2, Arrays.asList(newHighValue2));
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addOp2a);
		elements.add(substHighValues);
		elements.add(substHighValues2);
		this.deltas.add(new Delta(elements));
		
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertTrue(sdec.perform(null, this.testHost));
	}

	@Test
	@Ignore
	public final void testSubstNewOperationFail1() {
		init(null);
		AddElement addOp2a = new AddElement(this.isupplier, UMLPackage.eINSTANCE.getOperation(), null);
		addOp2a.addKeyValuePair(NAME, OP2A);
		
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		TaggedValue highValue = criticalApp.getTaggedValue(HIGH);
		AddElement newHighValue = new AddElement(criticalApp, UMLPackage.eINSTANCE.getProperty(), null);
		newHighValue.addKeyValuePair(NAME, HIGH);
		newHighValue.addKeyValuePair(VALUE, OP2A);
		SubstElement substHighValues = new SubstElement(highValue, Arrays.asList(newHighValue));
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addOp2a);
		elements.add(substHighValues);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(1, sdec.getViolations().size());
	}

	@Test
	@Ignore
	public final void testSubstNewOperationFail2() {
		init(null);
		AddElement addOp2a = new AddElement(this.isupplier, UMLPackage.eINSTANCE.getOperation(), null);
		addOp2a.addKeyValuePair(NAME, OP2A);
		
		StereotypeApplication highApp = UMLHelper.getStereotypeApplication(this.dep2, HIGH);
		DelElement delHighStereo = new DelElement(highApp);
		
		StereotypeApplication criticalApp = UMLHelper.getStereotypeApplication(this.supplier2, CRITICAL);
		TaggedValue highValue = criticalApp.getTaggedValue(HIGH);
		AddElement newHighValue = new AddElement(criticalApp, UMLPackage.eINSTANCE.getProperty(), null);
		newHighValue.addKeyValuePair(NAME, HIGH);
		newHighValue.addKeyValuePair(VALUE, OP2A);
		SubstElement substHighValues = new SubstElement(highValue, Arrays.asList(newHighValue));
		
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(addOp2a);
		elements.add(delHighStereo);
		elements.add(substHighValues);
		this.deltas.add(new Delta(elements));
		
		SecureDependencyEvolutionCheck sdec = new SecureDependencyEvolutionCheck();
		assertFalse(sdec.perform(null, this.testHost));
		assertEquals(2, sdec.getViolations().size());
	}
	
	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
