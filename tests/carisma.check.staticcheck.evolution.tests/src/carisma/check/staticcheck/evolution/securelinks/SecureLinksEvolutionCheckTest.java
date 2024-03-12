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
package carisma.check.staticcheck.evolution.securelinks;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceImpl;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil.Copier;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.UMLPackage;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.check.staticcheck.securelinks.SecureLinks;
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
import carisma.evolution.uml2.ModifierMap;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;
import carisma.profile.umlsec.UMLsec;
import carisma.profile.umlsec.UMLsecUtil;

@Ignore
public class SecureLinksEvolutionCheckTest {
    
    /**
     * Constant String for the name of the key 'name'.
     */
    private static final String NAME = "name";
	
	/** 
     * Constant String for the name of the 'UMLchange' profile.
     */
    private static final String UML_CHANGE = "UMLchange";
    /** 
     * Constant String for the name of the 'UMLsec' profile.
     */
    private static final String UML_SEC = "UMLsec";
    
    /** 
     * Constant String for the name of an Element in the test-model.
     */
    private static final String AB_LINK = "abLink";
    
    /** 
     * Constant String for the name of an Element in the test-model.
     */
    private static final String DEP = "dep";

    /** 
     * Constant String for the qualified name of the UMLsec stereotype.
     */
    private static final String UML_SEC_HIGH = "UMLsec::high";

    /** 
     * Constant String for the qualified name of the UMLsec stereotype.
     */
    private static final String UML_SEC_SECRECY = "UMLsec::secrecy";

    /** 
     * Constant String for the qualified name of the UMLsec stereotype.
     */
    private static final String UML_SEC_LAN = "UMLsec::LAN";
    
	
	private AnalysisHost testHost = new TestHost();
	
	private String filepath = "resources/models/securelinks";
	
	private ResourceSet rs = new ResourceSetImpl();
	
	Resource modelres = null;
	
	List<Delta> deltas = null;
	
	ModifierMap modifierMap = null;
	
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
	
	@Test
	public final void testDeleteLink() throws IOException {
		this.deltas = new ArrayList<>();
		loadModel("testSLDeleteLink.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		Logger.log(LogLevel.DEBUG, model.toString());
		assertNotNull(model);
		assertNotNull(model.getAppliedProfile(UML_CHANGE));
		assertNotNull(model.getAppliedProfile(UML_SEC));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));
		Logger.log(LogLevel.DEBUG, model.getMembers().toString());
		Package package1 = (Package) model.getOwnedElements().get(0);
		assertNotNull(package1);
		CommunicationPath commPath = (CommunicationPath) package1.getOwnedMember(AB_LINK);
		assertNotNull(commPath);
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(new DelElement(commPath));
		this.deltas.add(new Delta(elements));
		assertEquals(2, model.getAllAppliedProfiles().size());
		assertEquals(2, commPath.getAppliedStereotypes().size());
		SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
		assertFalse(evoCheck.perform(null, this.testHost));
		Dependency dep = (Dependency) package1.getMember(DEP);
		assertEquals(1, SecureLinks.getThreats(commPath).size());
		assertNotNull(dep);
		assertFalse(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		assertTrue(UMLHelper.unapplyStereotype(dep, UML_SEC_HIGH));
		assertTrue(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		assertNotNull(UMLHelper.applyStereotype(dep, UML_SEC_SECRECY));
		assertTrue(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		evoCheck = new SecureLinksEvolutionCheck();
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		assertFalse(evoCheck.perform(null, this.testHost));		
		assertTrue(UMLHelper.unapplyStereotype(dep, UML_SEC_SECRECY));
		evoCheck = new SecureLinksEvolutionCheck();
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		assertTrue(evoCheck.perform(null, this.testHost));
	}
	
	@Test
	public final void testDeleteLinktype() throws IOException {
		this.deltas = new ArrayList<>();
		loadModel("testSLDeleteLinktype.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		assertNotNull(model.getAppliedProfile(UML_CHANGE));
		assertNotNull(model.getAppliedProfile(UML_SEC));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));
		Package package1 = (Package) model.getOwnedElements().get(0);
		assertNotNull(package1);
		CommunicationPath commPath = (CommunicationPath) package1.getMember(AB_LINK);
		assertNotNull(commPath);
		StereotypeApplication lan = UMLHelper.getStereotypeApplication(commPath, "LAN");
		assertNotNull(lan);
		assertEquals(2, model.getAllAppliedProfiles().size());
		assertEquals(2, commPath.getAppliedStereotypes().size());
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(new DelElement(lan));
		this.deltas.add(new Delta(elements));
		SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
		assertFalse(evoCheck.perform(null, this.testHost));
		Dependency dep = (Dependency) package1.getMember(DEP);
		assertNotNull(dep);
		assertEquals(1, SecureLinks.getThreats(commPath).size());
		assertFalse(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		assertTrue(UMLHelper.unapplyStereotype(dep, UML_SEC_HIGH));
		assertTrue(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		assertNotNull(UMLHelper.applyStereotype(dep, UML_SEC_SECRECY));
		assertTrue(SecureLinks.compliesWithRequirements(commPath, dep).isEmpty());
		evoCheck = new SecureLinksEvolutionCheck();
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		assertFalse(evoCheck.perform(null, this.testHost));
		assertTrue(UMLHelper.unapplyStereotype(dep, UML_SEC_SECRECY));
		evoCheck = new SecureLinksEvolutionCheck();
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		assertTrue(evoCheck.perform(null, this.testHost));
	}
	
	@Test
	public final void testDeleteSecureLinks() throws IOException {
		this.deltas = new ArrayList<>();
		loadModel("testSLDeleteSecureLinks.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		assertNotNull(model.getAppliedProfile(UML_CHANGE));
		assertNotNull(model.getAppliedProfile(UML_SEC));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));
		Package package1 = (Package) model.getOwnedElements().get(0);
		assertNotNull(package1);
		CommunicationPath commPath = (CommunicationPath) package1.getMember(AB_LINK);
		assertNotNull(commPath);
		assertEquals(2, model.getAllAppliedProfiles().size());
		assertEquals(1, commPath.getAppliedStereotypes().size());
		List<DeltaElement> elements = new ArrayList<>();
		StereotypeApplication secureLinks = UMLHelper.getStereotypeApplication(model, "secure links");
		assertNotNull(secureLinks);
		elements.add(new DelElement(secureLinks));
		this.deltas.add(new Delta(elements));
		SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
		assertTrue(evoCheck.perform(null, this.testHost));
	}
	
	@Test
	public final void testDeleteAdversary() throws IOException {
		this.deltas = new ArrayList<>();
		loadModel("testSLDeleteAdversary.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		assertNotNull(model.getAppliedProfile(UML_CHANGE));
		assertNotNull(model.getAppliedProfile(UML_SEC));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));
		Package package1 = (Package) model.getOwnedElements().get(0);
		assertNotNull(package1);
		CommunicationPath commPath = (CommunicationPath) package1.getMember(AB_LINK);
		assertNotNull(commPath);
		assertEquals(2, model.getAllAppliedProfiles().size());
		List<DeltaElement> elements = new ArrayList<>();
		StereotypeApplication secureLinks = UMLHelper.getStereotypeApplication(model, "secure links");
		assertNotNull(secureLinks);
		TaggedValue adversary = secureLinks.getTaggedValue("adversary");
		assertNotNull(adversary);
		elements.add(new DelElement(adversary));
		this.deltas.add(new Delta(elements));
		SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
		assertTrue(evoCheck.perform(null, this.testHost));
	}
	
	@Test
	public final void testAddRequirement() throws IOException {
		this.deltas = new ArrayList<>();
		loadModel("testSLAddRequirement.uml");
		try {
			Model model = (Model) this.modelres.getContents().get(0);
			assertNotNull(model);
			assertNotNull(model.getAppliedProfile(UML_CHANGE));
			assertNotNull(model.getAppliedProfile(UML_SEC));
			assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));		
			List<DeltaElement> elements = new ArrayList<>();
			Dependency dep = UMLHelper.getElementOfNameAndType(model, DEP, Dependency.class);
			SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
			AddElement newRequirement = new AddElement(dep, UMLPackage.eINSTANCE.getStereotype(), null);
			newRequirement.addKeyValuePair(NAME, UML_SEC_HIGH);
			elements.add(newRequirement);
			this.deltas.add(new Delta(elements));
			assertFalse(evoCheck.perform(null, this.testHost));
			Package package1 = (Package) model.getOwnedElements().get(0);
			assertNotNull(package1);
			CommunicationPath commPath = (CommunicationPath) package1.getMember(AB_LINK);
			assertNotNull(commPath);
			assertTrue(UMLHelper.unapplyStereotype(commPath, "UMLsec::Internet"));
			assertNotNull(UMLHelper.applyStereotype(commPath, UML_SEC_LAN));
			evoCheck = new SecureLinksEvolutionCheck();
			this.deltas.clear();
			this.deltas.add(new Delta(elements));
			assertTrue(evoCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	@Test
	public final void testAddLinktype() throws IOException {
		this.deltas = new ArrayList<>();		
		loadModel("testSLAddLinkType.uml");
		try {
			Model model = (Model) this.modelres.getContents().get(0);
			assertNotNull(model);
			assertNotNull(model.getAppliedProfile(UML_CHANGE));
			assertNotNull(model.getAppliedProfile(UML_SEC));
			assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));		
			CommunicationPath commPath = UMLHelper.getElementOfNameAndType(model, AB_LINK, CommunicationPath.class);
			assertNotNull(commPath);
			AddElement newLinktype = new AddElement(commPath, UMLPackage.eINSTANCE.getStereotype(), null);
			newLinktype.addKeyValuePair(NAME, UML_SEC_LAN);
			List<DeltaElement> elements = new ArrayList<>();
			elements.add(newLinktype);
			this.deltas.add(new Delta(elements));
			SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
			assertTrue(evoCheck.perform(null, this.testHost));
			Dependency dep = UMLHelper.getElementOfNameAndType(model, DEP, Dependency.class);
			assertNotNull(dep);
			StereotypeApplication high = UMLHelper.applyStereotype(dep, UML_SEC_HIGH);
			assertNotNull(high);
			this.deltas.clear();
			this.deltas.add(new Delta(elements));
			evoCheck = new SecureLinksEvolutionCheck();
			assertFalse(evoCheck.perform(null, this.testHost));		
		}
		catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	@Test
	public final void testAddDeployment() throws IOException {
		this.deltas = new ArrayList<>();		
		loadModel("testSLAddDeployment.uml");
		try {
			Model model = (Model) this.modelres.getContents().get(0);
			assertNotNull(model);
			assertNotNull(model.getAppliedProfile(UML_CHANGE));
			assertNotNull(model.getAppliedProfile(UML_SEC));
			assertEquals("default", SecureLinks.getAttacker(model));
			assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));
			Node node1 = UMLHelper.getElementOfNameAndType(model, "Node1", Node.class);
			assertNotNull(node1);
			AddElement newDeployment = new AddElement(node1, UMLPackage.eINSTANCE.getDeployment(), null);
			newDeployment.addKeyValuePair(NAME, "NewDeployment");		
			newDeployment.addKeyValuePair("location", "Node1");		
			newDeployment.addKeyValuePair("deployedArtifact", "Artifact1");		
			List<DeltaElement> elements = new ArrayList<>();
			elements.add(newDeployment);
			this.deltas.add(new Delta(elements));
			SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
			assertFalse(evoCheck.perform(null, this.testHost));
			Package package1 = (Package) model.getOwnedElements().get(0);
			assertNotNull(package1);
			CommunicationPath commPath = (CommunicationPath) package1.getMember(AB_LINK);
			Dependency dep = (Dependency) package1.getMember(DEP);
			assertNotNull(commPath);
			assertNotNull(dep);
			assertTrue(UMLHelper.unapplyStereotype(commPath, "UMLsec::Internet"));
			assertNotNull(UMLHelper.applyStereotype(commPath, UML_SEC_LAN));
			this.deltas.clear();
			this.deltas.add(new Delta(elements));
			evoCheck = new SecureLinksEvolutionCheck();
			assertTrue(evoCheck.perform(null, this.testHost));
			assertTrue(UMLHelper.unapplyStereotype(commPath, UML_SEC_LAN));
			assertNotNull(UMLHelper.applyStereotype(commPath, "UMLsec::encrypted"));
			this.deltas.clear();
			this.deltas.add(new Delta(elements));
			evoCheck = new SecureLinksEvolutionCheck();
			assertTrue(evoCheck.perform(null, this.testHost));
			assertNotNull(UMLHelper.applyStereotype(dep, UML_SEC_HIGH));
			this.deltas.clear();
			this.deltas.add(new Delta(elements));
			evoCheck = new SecureLinksEvolutionCheck();
			assertFalse(evoCheck.perform(null, this.testHost));
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());
		}
	}
	
	@Test
	public final void testSLAddNodeWithDeployment() throws IOException {
		this.deltas = new ArrayList<>();		
		loadModel("testSLAddNodeWithDeploymentNoLink.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		assertNotNull(model.getAppliedProfile(UML_CHANGE));
		assertNotNull(model.getAppliedProfile(UML_SEC));
		assertEquals("default", SecureLinks.getAttacker(model));
		assertTrue(UMLsecUtil.hasStereotype(model, UMLsec.SECURE_LINKS));		
		AddElement newNode = new AddElement(model, UMLPackage.eINSTANCE.getNode(), null);
		newNode.addKeyValuePair(NAME, "newNode");
		AddElement newDeployment = new AddElement(null, UMLPackage.eINSTANCE.getDeployment(), newNode);
		newDeployment.addKeyValuePair(NAME, "newDeployment");
		newDeployment.addKeyValuePair("location", "newNode");		
		newDeployment.addKeyValuePair("deployedArtifact", "Artifact2");
		newNode.addContainedElement(newDeployment);
		List<DeltaElement> elements = new ArrayList<>();
		elements.add(newNode);
		this.deltas.add(new Delta(elements));
		SecureLinksEvolutionCheck evoCheck = new SecureLinksEvolutionCheck();
		assertFalse(evoCheck.perform(null, this.testHost));
		AddElement newCommPath = new AddElement(null, UMLPackage.eINSTANCE.getCommunicationPath(), newNode);
		newCommPath.addKeyValuePair(NAME, "newCommPath");
		newCommPath.addKeyValuePair("source", "newNode");		
		newCommPath.addKeyValuePair("target", "Node1");
		newNode.addContainedElement(newCommPath);
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		evoCheck = new SecureLinksEvolutionCheck();		
		assertFalse(evoCheck.perform(null, this.testHost));
		AddElement newInternetStereotype = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), newCommPath);
		newInternetStereotype.addKeyValuePair(NAME, "UMLsec::internet");
		newCommPath.addContainedElement(newInternetStereotype);
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		evoCheck = new SecureLinksEvolutionCheck();		
		assertFalse(evoCheck.perform(null, this.testHost));
		newCommPath.removeContainedElement(newInternetStereotype);
		AddElement newLanStereotype = new AddElement(null, UMLPackage.eINSTANCE.getStereotype(), newCommPath);
		newLanStereotype.addKeyValuePair(NAME, UML_SEC_LAN);
		newCommPath.addContainedElement(newLanStereotype);
		this.deltas.clear();
		this.deltas.add(new Delta(elements));
		evoCheck = new SecureLinksEvolutionCheck();		
		assertTrue(evoCheck.perform(null, this.testHost));
	}
	
	@Test
	public final void testCopyModel() throws IOException {
		loadModel("testSLAddRequirement.uml");
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		Copier copier = new Copier();
		URI oldUri = this.modelres.getURI();
		URI newUri = URI.createURI(oldUri.toString().concat("modified"));
		Resource newModel = new ResourceImpl(newUri);
		copier.clear();
		Collection<EObject> allObjects = copier.copyAll(this.modelres.getContents());
		copier.copyReferences();
		newModel.getContents().addAll(allObjects);
		Model modelCopy = (Model) copier.get(model);
		assertNotNull(modelCopy);
		assertTrue(UMLsecUtil.hasStereotype(model));
		assertTrue(UMLsecUtil.hasStereotype(modelCopy));
	}

	@After
	public void unloadModel(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}

	private class TestHost implements AnalysisHost {
	
		public TestHost() {
			// TODO Auto-generated constructor stub
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
			return SecureLinksEvolutionCheckTest.this.modelres;
		}
	
		@Override
		public String getCurrentModelFilename() {
			return SecureLinksEvolutionCheckTest.this.modelres.getURI().toFileString();
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
			if (registerName.equals(SecureLinksEvolutionCheck.PRECONDITION_DELTAS_REGISTER_KEY)) {
				return new DeltaList(SecureLinksEvolutionCheckTest.this.deltas);
			} else if (registerName.equals(SecureLinksEvolutionCheck.PRECONDITION_MODIFIERS_REGISTRY_KEY)) {
				return SecureLinksEvolutionCheckTest.this.modifierMap;
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
		}
	
		@Override
		public File getFileToBeWritten(final File file)
				throws UserAbortedAnalysisException {
			// TODO Auto-generated method stub
			return file;
		}
	}
}
