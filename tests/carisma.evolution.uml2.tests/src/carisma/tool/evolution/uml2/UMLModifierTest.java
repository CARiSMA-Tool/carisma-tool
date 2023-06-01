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
package carisma.tool.evolution.uml2;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.emf.ecore.util.EcoreUtil.Copier;
import org.eclipse.uml2.uml.Artifact;
import org.eclipse.uml2.uml.Class;
import org.eclipse.uml2.uml.CommunicationPath;
import org.eclipse.uml2.uml.Dependency;
import org.eclipse.uml2.uml.Deployment;
import org.eclipse.uml2.uml.Element;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;
import org.eclipse.uml2.uml.Node;
import org.eclipse.uml2.uml.Operation;
import org.eclipse.uml2.uml.Package;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.Property;
import org.eclipse.uml2.uml.Stereotype;
import org.junit.After;
import org.junit.Ignore;
import org.junit.Test;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.AddElement;
import carisma.evolution.DelElement;
import carisma.evolution.Delta;
import carisma.evolution.DeltaElement;
import carisma.evolution.uml2.UMLModifier;
import carisma.evolution.uml2.UMLModifierElementFactory;
import carisma.modeltype.uml2.StereotypeApplication;
import carisma.modeltype.uml2.TaggedValue;
import carisma.modeltype.uml2.UMLDeploymentHelper;
import carisma.modeltype.uml2.UMLHelper;
import carisma.modeltype.uml2.exceptions.InvalidMetaclassException;
import carisma.modeltype.uml2.exceptions.ModelElementNotFoundException;


public class UMLModifierTest {

	
	private ResourceSet rs = new ResourceSetImpl();
	
	private Resource modelres = null;

	private String 	filepath = "resources/models/modifier";
	
	/**
	 * Constant name of the Model for simple deletion deployments.
	 */
	private static final String SIMPLE_DELETION_DEPLOYMENTS_MODEL = "testSimpleDeletionDeployments.uml";
	
	/**
	 * Constant name of the Model for simple deletion deployments package.
	 */
	private static final String SIMPLE_DELETION_DEPLOYMENTS_PACKAGE_MODEL = "testSimpleDeletionDeploymentsPackage";
	
	/**
	 * Constant Model name.
	 */
	private static final String CREATION_MODEL = "testCreation.uml";
	
	/** Constant names in the model.
	 */
	private static final String CLASS1 = "Class1";
	private static final String NEW_CLASS = "newClass";
	private static final String NODE1 = "Node1";
	private static final String NODE2 = "Node2";
	private static final String NODE3 = "Node3";
	private static final String DEPENDENCY = "dep";
	private static final String AB_LINK = "abLink";
	private static final String ARTIFACT1 = "Artifact1";
	private static final String ARTIFACT2 = "Artifact2";
	private static final String PACKAGE1 = "Package1";
	private static final String NAME = "name";
	
	/**
	 * Constant name for the Tagged Value 'adversary'.
	 */
	private static final String ADVERSARY = "adversary";
	
	/**
	 * Constant element name.
	 */
	private static final String TEST_CREATION_PACKAGE = "testCreationPackage";
	
	
	public final void loadModel(final String testmodelname) throws IOException {
		File testmodelfile = new File(this.filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		this.modelres = this.rs.createResource(URI.createFileURI(testmodelfile.getAbsolutePath()));
		this.modelres.load(Collections.EMPTY_MAP);
	}
		
	@Test
	public final void testDeleteOneElement() throws IOException {
		loadModel("testSimpleDeletionOneElement.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember("testSimpleDeletionOneElementPackage");
		assertNotNull(pkg);
		NamedElement delNode = pkg.getMember(NODE1);
		assertNotNull(delNode);
		DelElement del = new DelElement(delNode); 
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(del);
		assertNotNull(delNode);
		delNode = pkg.getMember(NODE1);
		assertNull(delNode);
	}
	
	@Test
	public final void testDeleteConnectedElementNode() throws IOException {
		loadModel("testSimpleDeletionConnectedElement.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember("testSimpleDeletionConnectedElementPackage");
		assertNotNull(pkg);
		NamedElement delNode = pkg.getMember(NODE1);
		assertNotNull(delNode);
		NamedElement removedLink = pkg.getMember(AB_LINK);
		assertNotNull(removedLink);
		DelElement del = new DelElement(delNode);
		List<EObject> alsoDeleted = new ArrayList<>();
		alsoDeleted.add(removedLink);
		del.replaceAccompanyingDeletions(alsoDeleted);
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(del);
		assertNotNull(delNode);
		assertNotNull(removedLink);
		assertFalse(model.allOwnedElements().contains(delNode));
		delNode = pkg.getMember(NODE1);
		assertNull(delNode);
		NamedElement otherNode = pkg.getMember(NODE2);
		assertNotNull(otherNode);
		Node otherN = (Node) otherNode;
		assertEquals(0, otherN.getCommunicationPaths().size());
		assertFalse(model.allOwnedElements().contains(removedLink));
	}
	
	@Test
	public final void testDeleteConnection() throws IOException {
		loadModel("testSimpleDeletionConnectedElement.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember("testSimpleDeletionConnectedElementPackage");
		assertNotNull(pkg);
		NamedElement removedLink = pkg.getMember(AB_LINK);
		assertNotNull(removedLink);
		DelElement del = new DelElement(removedLink);
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(del);
		assertNotNull(removedLink);
		removedLink = pkg.getMember(AB_LINK);
		assertNull(removedLink);
		NamedElement source = pkg.getMember(NODE1);
		NamedElement target = pkg.getMember(NODE2);
		Node sourceNode = (Node) source;
		Node targetNode = (Node) target;
		assertTrue(sourceNode.getCommunicationPaths().isEmpty());
		assertTrue(targetNode.getCommunicationPaths().isEmpty());
	}
	
	@Test
	public final void testDeleteDeploymentsDeployment() throws IOException {
		loadModel(SIMPLE_DELETION_DEPLOYMENTS_MODEL);
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember(SIMPLE_DELETION_DEPLOYMENTS_PACKAGE_MODEL);
		assertNotNull(pkg);
		Artifact arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		Deployment deploy = null;
		for (Deployment d : UMLDeploymentHelper.getDeployments(arti1)) {
			deploy = d;
			break;
		}
		assertNotNull(deploy);
		DelElement delDeployment = new DelElement(deploy);
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(delDeployment);
		assertNotNull(deploy);
		assertEquals(0, UMLDeploymentHelper.getDeployments(arti1).size());
		Node location = (Node) pkg.getMember(NODE1);
		assertNotNull(location);
		arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		assertTrue(UMLDeploymentHelper.getDeploymentLocations(arti1).isEmpty());
		assertTrue(UMLDeploymentHelper.getDeployedArtifacts(location).isEmpty());
	}
	
	@Test
	public final void testDeleteDeploymentsNode() throws IOException {
		loadModel(SIMPLE_DELETION_DEPLOYMENTS_MODEL);
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember(SIMPLE_DELETION_DEPLOYMENTS_PACKAGE_MODEL);
		assertNotNull(pkg);
		Artifact arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		Node location = (Node) pkg.getMember(NODE1);
		assertNotNull(location);
		Deployment deploy = null;
		for (Deployment d : UMLDeploymentHelper.getDeployments(arti1)) {
			deploy = d;
			break;
		}
		assertNotNull(deploy);
		DelElement delLocation = new DelElement(location);
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(delLocation);
		assertNotNull(deploy);
		assertNotNull(location.getDeployment("arti1Deploy"));
		arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		assertTrue(UMLDeploymentHelper.getDeploymentLocations(arti1).isEmpty());
		assertTrue(UMLDeploymentHelper.getDeployedArtifacts(location).isEmpty());
	}
	
	@Test
	public final void testDeleteDeploymentsArtifact() throws IOException {
		loadModel(SIMPLE_DELETION_DEPLOYMENTS_MODEL);
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Package pkg = (Package) model.getMember(SIMPLE_DELETION_DEPLOYMENTS_PACKAGE_MODEL);
		assertNotNull(pkg);
		Artifact arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		Node location = (Node) pkg.getMember(NODE1);
		assertNotNull(location);
		Deployment deploy = null;
		for (Deployment d : UMLDeploymentHelper.getDeployments(arti1)) {
			deploy = d;
			break;
		}
		assertNotNull(deploy);
		DelElement delArtifact = new DelElement(arti1);
		delArtifact.addDeletion(deploy);
		mod.deleteElement(delArtifact);
		deploy = location.getDeployment("arti1Deploy");
		assertNull(deploy);
		arti1 = (Artifact) model.getMember(ARTIFACT1);
		assertNull(arti1);
		assertTrue(UMLDeploymentHelper.getDeployedArtifacts(location).isEmpty());
	}
	
	@Test
	public final void testDeleteDeploymentsDependency() throws IOException {
		loadModel(SIMPLE_DELETION_DEPLOYMENTS_MODEL);
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		Package pkg = (Package) model.getMember(SIMPLE_DELETION_DEPLOYMENTS_PACKAGE_MODEL);
		assertNotNull(pkg);
		Artifact arti1 = (Artifact) pkg.getMember(ARTIFACT1);
		assertNotNull(arti1);
		Artifact arti2 = (Artifact) pkg.getMember(ARTIFACT2);
		assertNotNull(arti2);
		Dependency dep = (Dependency) pkg.getMember(DEPENDENCY);
		assertNotNull(dep);
		DelElement delDep = new DelElement(dep);
		UMLModifier mod = new UMLModifier(this.modelres);
		mod.deleteElement(delDep);
		assertNotNull(arti1);
		assertNotNull(arti2);
		assertNotNull(dep);
		assertTrue(arti1.getClientDependencies().isEmpty());
		dep = (Dependency) pkg.getMember(DEPENDENCY);
		assertNull(dep);
	}
	
	@Test
	public final void testDeleteSuperPackage() throws IOException {
		loadModel("testDeleteNested.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Package pkg1 = (Package) model.getMember(PACKAGE1);
		assertNotNull(pkg1);
		Class class1 = (Class) pkg1.getOwnedMember(CLASS1);
		assertNotNull(class1);
		Package subpkg1 = (Package) pkg1.getOwnedMember("Subpackage1");
		assertNotNull(subpkg1);
		DelElement delPkg = new DelElement(pkg1);
		assertEquals(2, UMLHelper.getAllElementsOfType(model, Package.class).size());
		assertEquals(3, UMLHelper.getAllElementsOfType(model, Class.class).size());
		mod.deleteElement(delPkg);
		assertEquals(0, UMLHelper.getAllElementsOfType(model, Package.class).size());
		assertEquals(0, UMLHelper.getAllElementsOfType(model, Class.class).size());
		assertNotNull(pkg1);
		assertNotNull(class1);
		assertNotNull(subpkg1);
		class1 = (Class) pkg1.getOwnedMember(CLASS1);
		subpkg1 = (Package) pkg1.getOwnedMember("Subpackage1");		
		pkg1 = (Package) model.getMember(PACKAGE1);
		assertNull(pkg1);
		assertNotNull(class1);
		assertNotNull(subpkg1);
	}
	
	@Test
	public final void testDeleteSubPackage() throws IOException {
		loadModel("testDeleteNested.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Package pkg1 = (Package) model.getMember(PACKAGE1);
		assertNotNull(pkg1);
		Class class1 = (Class) pkg1.getOwnedMember(CLASS1);
		assertNotNull(class1);
		Package subpkg1 = (Package) pkg1.getOwnedMember("Subpackage1");
		assertNotNull(subpkg1);
		Class subclass = (Class) subpkg1.getOwnedMember("Subclass1");
		assertNotNull(subclass);
		DelElement delSubPkg = new DelElement(subpkg1);
		assertEquals(2, UMLHelper.getAllElementsOfType(model, Package.class).size());
		assertEquals(3, UMLHelper.getAllElementsOfType(model, Class.class).size());
		mod.deleteElement(delSubPkg);
		assertEquals(1, UMLHelper.getAllElementsOfType(model, Package.class).size());
		assertEquals(2, UMLHelper.getAllElementsOfType(model, Class.class).size());		
	}
	
	@Test
	@Ignore
	public final void testDeleteTaggedValue() throws IOException {
		loadModel("testDeleteStereotype.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Stereotype stereo = model.getAppliedStereotypes().get(0);
		assertNotNull(stereo);
		StereotypeApplication stapp = new StereotypeApplication(stereo, model);
		assertNotNull(stapp);
		Property tag = stereo.getAttribute(ADVERSARY, null);
		assertNotNull(tag);
		assertEquals("insider", model.getValue(stereo, ADVERSARY));
		TaggedValue tagValue = new TaggedValue(tag, stapp);
		DelElement del = new DelElement(tagValue);
		mod.deleteElement(del);
		assertEquals(null, model.getValue(stereo, ADVERSARY));
	}
	
	@Test
	@Ignore
	public final void testDeleteStereotype() throws IOException {
		loadModel("testDeleteStereotype.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Package pkg = (Package) model.getMember("testDeleteStereotypePackage");
		assertNotNull(pkg);
		Dependency dep = (Dependency) pkg.getMember(DEPENDENCY);
		assertNotNull(dep);
		Stereotype stereo = dep.getAppliedStereotypes().get(0);
		StereotypeApplication stapp = new StereotypeApplication(stereo, dep);
		DelElement del = new DelElement(stapp);
		assertEquals(4, dep.getAppliedStereotypes().size());
		assertTrue(dep.isStereotypeApplied(stereo));
		mod.deleteElement(del);
		assertEquals(3, dep.getAppliedStereotypes().size());
		assertFalse(dep.isStereotypeApplied(stereo));
	}
	
	@Test
	public final void testCreateNode() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			AddElement add = new AddElement(model, UMLHelper.getMetaClass("Node"), null);
			add.addKeyValuePair(NAME, "newNode");
			assertEquals(3, UMLHelper.getAllElementsOfType(model, Node.class).size());
			Node newElement = (Node) UMLModifierElementFactory.createElement(add);
			assertEquals(4, UMLHelper.getAllElementsOfType(model, Node.class).size());
			assertEquals(model, newElement.getOwner());
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}
	}
	
	@Test
	public final void testCreateDependency() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			Package pkg = (Package) model.getMember(TEST_CREATION_PACKAGE);
			assertNotNull(pkg);
			AddElement add = new AddElement(model, UMLHelper.getMetaClass("Dependency"), null);
			Artifact arti2 = (Artifact) pkg.getMember(ARTIFACT2);
			assertNotNull(arti2);
			Artifact arti3 = (Artifact) pkg.getMember("Artifact3");
			assertNotNull(arti3);
			add.addKeyValuePair(NAME, "newDep");
			add.addKeyValuePair("client", arti2);
			add.addKeyValuePair("supplier", arti3);
			assertEquals(1, UMLDeploymentHelper.getAllDependencies(model).size());
			assertEquals(0, arti2.getClientDependencies().size());
			Dependency newDep = (Dependency) UMLModifierElementFactory.createElement(add);
			assertEquals(1, arti2.getClientDependencies().size());
			assertEquals(1, newDep.getClients().size());
			assertEquals(1, newDep.getSuppliers().size());
			Dependency dep = (Dependency) pkg.getMember(DEPENDENCY);
			assertNotNull(dep);
			assertEquals(model, dep.getOwner().getOwner());
			Logger.log(LogLevel.DEBUG, newDep.getOwner().toString());
			assertEquals(2, UMLDeploymentHelper.getAllDependencies(model).size());
			assertEquals(model, newDep.getOwner());
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	public final void testCreateLink() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			AddElement add = new AddElement(model, UMLHelper.getMetaClass("CommunicationPath"), null);
			Logger.log(LogLevel.DEBUG, add.getMetaClass().toString());
			add.addKeyValuePair(NAME, "newLink");
			add.addKeyValuePair("source", NODE2);
			add.addKeyValuePair("target", NODE3);
			assertEquals(1, UMLHelper.getAllElementsOfType(model, CommunicationPath.class).size());
			CommunicationPath newLink = (CommunicationPath) UMLModifierElementFactory.createElement(add);
			assertEquals(2, UMLDeploymentHelper.getNodes(newLink).size());
			assertEquals(model, newLink.getOwner().getOwner());	
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	public final void testCreateDeployment() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			Package pkg = (Package) model.getMember(TEST_CREATION_PACKAGE);
			assertNotNull(pkg);
			AddElement add = new AddElement(model, UMLHelper.getMetaClass("Deployment"), null);
			Node node2 = (Node) pkg.getMember(NODE2);
			assertNotNull(node2);
			Artifact arti2 = (Artifact) pkg.getMember(ARTIFACT2);
			assertNotNull(arti2);
			add.addKeyValuePair(NAME, "newDeployment");
			add.addKeyValuePair("deployedArtifact", ARTIFACT2);
			add.addKeyValuePair("location", NODE2);
			assertEquals(1, UMLHelper.getAllElementsOfType(model, Deployment.class).size());
			Deployment newDeploy = (Deployment) UMLModifierElementFactory.createElement(add);
			assertEquals(2, UMLHelper.getAllElementsOfType(model, Deployment.class).size());
			assertNotNull(newDeploy);
			assertEquals(1, node2.getDeployments().size());
			assertEquals(1, UMLDeploymentHelper.getDeployments(arti2).size());
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	@Ignore
	public final void testCreateStereotypeApplicaton() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			assertNotNull(model);
			Package pkg = (Package) model.getMember(TEST_CREATION_PACKAGE);
			assertNotNull(pkg);
			CommunicationPath commPath = (CommunicationPath) pkg.getMember(AB_LINK);
			assertNotNull(commPath);
			AddElement add = new AddElement(commPath, UMLHelper.getMetaClass("Stereotype"), null);
			add.addKeyValuePair(NAME, "encrypted");
			StereotypeApplication stapp = (StereotypeApplication) UMLModifierElementFactory.createElement(add);
			assertNotNull(stapp);
			assertNotNull(stapp.getExtendedElement());
			assertNotNull(stapp.getAppliedStereotype());
			assertTrue(stapp.getExtendedElement().isStereotypeApplied(stapp.getAppliedStereotype()));
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	@Ignore
	public final void testAddElementsWithContentElementPlusStereotype() throws IOException {
		loadModel(CREATION_MODEL);
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			UMLModifier mod = new UMLModifier(this.modelres);
			Package pkg = (Package) model.getMember(TEST_CREATION_PACKAGE);
			assertNotNull(pkg);
			AddElement add = new AddElement(model, UMLHelper.getMetaClass("CommunicationPath"), null);
			Logger.log(LogLevel.DEBUG, add.getMetaClass().toString());
			AddElement addStereo = new AddElement(null, UMLHelper.getMetaClass("Stereotype"), null);
			add.addContainedElement(addStereo);
			assertEquals(add, addStereo.getParent());
			add.addKeyValuePair(NAME, "link23");
			add.addKeyValuePair("source", NODE2);
			add.addKeyValuePair("target", NODE3);
			addStereo.addKeyValuePair(NAME, "LAN");
			mod.addElement(add);
			CommunicationPath commPath = (CommunicationPath) pkg.getMember("link23");
			Node node2 = (Node) pkg.getMember(NODE2);
			Node node3 = (Node) pkg.getMember(NODE3);
			assertNotNull(commPath);
			assertNotNull(node2);
			assertNotNull(node3);
			assertTrue(UMLDeploymentHelper.getNodes(commPath).contains(node2));
			assertTrue(UMLDeploymentHelper.getNodes(commPath).contains(node3));
			assertEquals(2, UMLDeploymentHelper.getNodes(commPath).size());
			assertEquals(1, commPath.getAppliedStereotypes().size());
			assertNotNull(commPath.getAppliedStereotype("UMLsec::LAN"));
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}				
	}
	
	@Test
	public final void testAddElementsWithContentClassWithContent() throws IOException {
		loadModel("testAddElementsWithContent.uml");
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			UMLModifier mod = new UMLModifier(this.modelres);
			AddElement addClass = new AddElement(model, UMLHelper.getMetaClass("Class"), null);
			AddElement addOperation = new AddElement(null, UMLHelper.getMetaClass("Operation"), null);
			AddElement addParameter = new AddElement(null, UMLHelper.getMetaClass("Parameter"), null);
			addClass.addContainedElement(addOperation);
			addOperation.addContainedElement(addParameter);
			addClass.addKeyValuePair(NAME, NEW_CLASS);
			addOperation.addKeyValuePair(NAME, "newOperation");
			addParameter.addKeyValuePair(NAME, "newParameter");
			addParameter.addKeyValuePair("type", UMLHelper.getPrimitiveType(model, "String"));
			mod.addElement(addClass);
			Class newClass = (Class) model.getMember(NEW_CLASS);
			assertNotNull(newClass);
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	@Ignore
	public final void testAddElementsWithContentOperationReturnTypeIsClass() throws IOException {
		loadModel("testAddElementsWithContent.uml");
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			UMLModifier mod = new UMLModifier(this.modelres);
			Package mainPackage = (Package) model.getMember("testAddElementsWithContent");
			assertNotNull(mainPackage);
			Package pkg = (Package) mainPackage.getMember(PACKAGE1);
			Class class1 = (Class) pkg.getMember(CLASS1);
			Logger.log(LogLevel.DEBUG, class1.getQualifiedName());
			assertNotNull(class1);
			AddElement addClass = new AddElement(model, UMLHelper.getMetaClass("Class"), null);
			AddElement addOperation = new AddElement(null, UMLHelper.getMetaClass("Operation"), null);
			addClass.addContainedElement(addOperation);
			addClass.addKeyValuePair(NAME, NEW_CLASS);
			addOperation.addKeyValuePair(NAME, "newOperation");
			addOperation.addKeyValuePair("type", "Package1::Class1");
			mod.addElement(addClass);
			Class newClass = (Class) model.getMember(NEW_CLASS);
			assertNotNull(newClass);
			assertEquals(1, newClass.getOwnedOperations().size());
			Operation newOp = newClass.getOwnedOperation("newOperation", null, null);
			assertNotNull(newOp);
			assertEquals(class1, newOp.getType());
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
	@Test
	@Ignore
	public final void testCopyDeltaDelElements() throws IOException {
		loadModel("testCopyDelta.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		UMLModifier mod = new UMLModifier(this.modelres);
		Package pkg = (Package) model.getMember("testCopyDeltaPackage");
		assertNotNull(pkg);
		Node node1 = (Node) pkg.getMember(NODE1);
		assertNotNull(node1);
		CommunicationPath commPath = (CommunicationPath) pkg.getMember(AB_LINK);
		assertNotNull(commPath);
		Deployment deploy2 = null;
		try {
			deploy2 = (Deployment) UMLHelper.getElementByName(model, "a1Deploy");
		} catch (ModelElementNotFoundException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail();
		}
		assertNotNull(deploy2);
		Artifact arti2 = (Artifact) pkg.getMember(ARTIFACT2);
		assertNotNull(arti2);
		Stereotype lanStereo = commPath.getAppliedStereotype("UMLsec::LAN");
		assertNotNull(lanStereo);
		Stereotype secureLinksStereo = model.getAppliedStereotype("UMLsec::secure links");
		assertNotNull(secureLinksStereo);
		StereotypeApplication lanApp = new StereotypeApplication(lanStereo, commPath);
		StereotypeApplication secLinksApp = new StereotypeApplication(secureLinksStereo, model);
		Property adversaryTag = secureLinksStereo.getAttribute(ADVERSARY, null);
		assertNotNull(adversaryTag);
		TaggedValue adversaryTagValue = new TaggedValue(adversaryTag, secLinksApp);
		DelElement delNode1 = new DelElement(node1);
		DelElement delComm = new DelElement(commPath);
		DelElement delDeploy = new DelElement(deploy2);
		DelElement delArti = new DelElement(arti2);
		DelElement delLan = new DelElement(lanApp);
		DelElement delAdversary = new DelElement(adversaryTagValue);
		List<DeltaElement> content = new ArrayList<>();
		content.add(delNode1);
		content.add(delComm);
		content.add(delDeploy);
		content.add(delArti);
		content.add(delLan);
		content.add(delAdversary);
		Delta delta = new Delta(content);
		Delta newDelta = mod.copyDelta(delta);
		assertNotNull(newDelta);
		assertEquals(delta.getContent().size(), newDelta.getContent().size());
		Copier cop = mod.getMapping();
		assertNotNull(cop);
		for (DelElement del : newDelta.getAllDeletions()) {
			EObject target = del.getTarget();
			if (target instanceof Element) {
				assertTrue(cop.containsValue(del.getTarget()));
			}
			if (target instanceof StereotypeApplication) {
				StereotypeApplication newApp = (StereotypeApplication) del.getTarget();
				assertTrue(cop.containsValue(newApp.getExtendedElement()));
			}
			if (target instanceof TaggedValue) {
				TaggedValue newTagValue = (TaggedValue) target;
				StereotypeApplication newApp = newTagValue.getCorrespondingApplication();
				assertTrue(cop.containsValue(newApp.getExtendedElement()));
			}
		}
	}
	
	@Test
	public final void testCopyDeltaAddElements() throws IOException {
		loadModel("testCopyDelta.uml");
		try {
			assertNotNull(this.modelres);
			Model model = (Model) this.modelres.getContents().get(0);
			UMLModifier mod = new UMLModifier(this.modelres);
			Package pkg = (Package) model.getMember("testCopyDeltaPackage");
			assertNotNull(pkg);
			Dependency dep = (Dependency) pkg.getMember(DEPENDENCY);
			assertNotNull(dep);
			AddElement addNode3 = new AddElement(model, UMLHelper.getMetaClass("Node"), null);
			AddElement addRequirement = new AddElement(dep, UMLHelper.getMetaClass("Stereotype"), null);
			addNode3.addKeyValuePair(NAME, NODE3);
			addRequirement.addKeyValuePair(NAME, "secrecy");
			List<DeltaElement> content = new ArrayList<>();
			content.add(addNode3);
			content.add(addRequirement);
			Delta delta = new Delta(content);
			Delta newDelta = mod.copyDelta(delta);
			assertNotNull(newDelta);
			assertEquals(delta.getContent().size(), newDelta.getContent().size());
			Copier cop = mod.getMapping();
			assertNotNull(cop);
			Iterator<AddElement> newAddIt = newDelta.getAllAdditions().iterator();
			AddElement newAddElem = newAddIt.next();
			for (AddElement oldAddElem : delta.getAllAdditions()) {
				assertEquals(newAddElem.getMetaClass(), oldAddElem.getMetaClass());
				assertEquals(newAddElem.getValues().size(), oldAddElem.getValues().size());
				assertEquals(newAddElem.getContent().size(), oldAddElem.getContent().size());
				if (newAddIt.hasNext()) {
					newAddElem = newAddIt.next();
				}
			}
		} catch (InvalidMetaclassException e) {
			Logger.log(LogLevel.ERROR, "", e);
			fail(e.getMessage());		
		}		
	}
	
//	@Test
	public void testModifying() {
//		Model model = UMLFactory.eINSTANCE.createModel();
//		Profile someProfile = UMLchangeUtil.getInstance().getProfile();
//		model.applyProfile(someProfile);
//		Package pkg = model.createNestedPackage("Paket");
//		Node node = (Node) pkg.createPackagedElement("leftNode", UMLPackage.eINSTANCE.getNode());
//		Node secondNode = (Node) pkg.createPackagedElement("rightNode", UMLPackage.eINSTANCE.getNode());
//		Node thirdNode = (Node) pkg.createPackagedElement("aloneNode", UMLPackage.eINSTANCE.getNode());
//		Stereotype add = someProfile.getOwnedStereotype("add");
//		node.applyStereotype(add);
//		Copier copier = new Copier();
//		Model copiedModel = (Model) copier.copy(model);
//		System.out.println("AllOwnedElements node:");
//		CommunicationPath commPath = UMLFactory.eINSTANCE.createCommunicationPath();
//		CommunicationPath bla = node.createCommunicationPath(
//				true, AggregationKind.NONE_LITERAL, "end1", 1, 1,
//				secondNode, true, AggregationKind.NONE_LITERAL, "end2", 1, 1);
//		System.out.println("Paths:");
//		for (CommunicationPath cp : node.getCommunicationPaths()) {
//			System.out.println(cp.getName());
//			System.out.println("EndTypes:");
//			for (Type type : cp.getEndTypes()) {
//				System.out.println(type.getName());
//			}
//			System.out.println("MemberEndTypes:");
//			cp.getMemberEnds();
//			for (Property prop : cp.getMemberEnds()) {
//				System.out.println(prop.getType().getName());
//			}
//		}
//		for (EStructuralFeature att : bla.eClass().getEAllStructuralFeatures()) {
//			if (att.getName().equals("memberEnd")) {
//				System.out.println("Hier");
//				((EList<EObject>) bla.eGet(att)).add(thirdNode);
//			}
//		}		
//		for (Property prop : bla.getMemberEnds()) {
//			System.out.println(prop.getType().getName());
//		}
//		commPath.setName("between");
//		for (EStructuralFeature att : node.eClass().getEAllStructuralFeatures()) {
////			System.out.println(att.getName());
//			if (att.getName().equals("nestedNode")) {
//				System.out.println("Nested Nodes vorher:");
//				for (Node nestedNode : node.getNestedNodes()) {
//					System.out.println(nestedNode.getName());
//				}
//				EList<EObject> list = (EList<EObject>) node.eGet(att);
//				list.add(secondNode);
//				System.out.println("Nested Nodes nachher:");
//				for (Node nestedNode : node.getNestedNodes()) {
//					System.out.println(nestedNode.getName());
//				}
//				System.out.println("Owner nachher:");
//				System.out.println(secondNode.getOwner().toString());
//			}
//		}
//		for (EStructuralFeature att : commPath.eClass().getEAllStructuralFeatures()) {
//			System.out.println(att.getName());
//		}
	}
	
	@Test
	@Ignore
	public final void testProfileApplication() throws IOException {
		loadModel("testProfileApplication.uml");
		assertNotNull(this.modelres);
		Model model = (Model) this.modelres.getContents().get(0);
		assertNotNull(model);
		assertEquals(3, model.getAllAppliedProfiles().size());
		for (Profile p : model.getAllAppliedProfiles()) {
			assertNotNull(p.getName());
		}
	}
	
	@After
	public void unloadModels(){
		for(Resource r : this.rs.getResources()){
			r.unload();
		}
	}
}
