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

package carisma.modeltype.bpmn2.extended;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;

import org.eclipse.bpmn2.Bpmn2Factory;
import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.junit.Test;

import carisma.modeltype.bpmn2.BPMN2ModelLoader;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;
import carisma.modeltype.bpmn2.extension.Lane;
import carisma.modeltype.bpmn2.extension.Task;
import carisma.modeltype.bpmn2.extension.util.ExtensionUtil;


/**
 * Test class.
 * @author Marcel Michel
 *
 */
public class MergeModelsTest {

	/**
	 * The model merger.
	 */
	private MergeModels merger = new MergeModels();
	
	/**
	 * The bpmn2 model loader.
	 */
	private BPMN2ModelLoader ml = null;
	
	/**
	 * The path to the model directory. 
	 */
	private String filepath = "resources/models/";
	
	/**
	 * Constant String for Exception handling.
	 */
	private static final String ROOT_DOCUMENT_ERROR = "Could not load extended document root";
	
	/**
	 * Method to load a bpmn2 model.
	 * @param testmodelname The model name
	 * @return If successful the model otherwise null
	 */
	public final Resource loadModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		if (ml == null) {
			ml = new BPMN2ModelLoader();
		}
		try {
			return ml.load(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
			return null;
		}
	}
	
	/**
	 * Method to load an extension model.
	 * @param testmodelname The model name
	 * @return If successful the model otherwise null
	 */
	public final Resource loadExtensionModel(final String testmodelname) {
		File testmodelfile = new File(filepath + File.separator + testmodelname);
		assertTrue(testmodelfile.exists());
		try {
			return ExtensionUtil.loadResource(testmodelfile);
		} catch (IOException e) {
			fail(e.getMessage());
			return null;
		}
	}
	
	/**
	 * Method helps to resolve a structural feature.
	 * @param name The name of the attribute
	 * @param obj The EObject which contains the attribute
	 * @return Returns the string value of the attribute
	 */
	public final String getStructuralFeature(final String name, final EObject obj) {
		if (obj != null) {
			EStructuralFeature sf = obj.eClass().getEStructuralFeature(name);
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature(name.substring(0, 1).toUpperCase(Locale.ENGLISH)
						+ name.substring(1));
			}
			if (sf == null) {
				sf = obj.eClass().getEStructuralFeature(name.toUpperCase(Locale.ENGLISH));
			}
			if (sf != null) {
				return String.valueOf(obj.eGet(sf));
			} else {
				return "[" + obj.eClass().getName() + "]";
			}
		} else {
			return "[no element]";
		}
	}
	
	/**
	 * Tests the merge method with null permutations.
	 */
	@Test
	public final void testMergeModelsNull() {
		Bpmn2Factory factory = Bpmn2Factory.eINSTANCE;
		Resource bpmn2res = factory.createDefinitions().eResource();
		
		assertNull(merger.run(bpmn2res, null));
		assertNull(merger.run(null, bpmn2res));
		assertNull(merger.run(null, null));
	}
	
	/**
	 * Tests if the extended model contains the complete bpmn2 instance. 
	 * Subset Relationship
	 */
	@Test
	public final void testMergeModels1() {
		Resource modelres = loadModel("trade.bpmn2");
		assertNotNull(modelres);
		
		DocumentRoot root = null;
		try {
			root = (DocumentRoot) modelres.getContents().get(0);
		} catch (Exception e) {
			fail("Could not load document root");
		}
		assertNotNull(root);
		
		Resource extensionModel = loadExtensionModel("trade.bpmn2extension");
		assertNotNull(extensionModel);
		
		Resource extendedModel = merger.run(modelres, extensionModel);
		
		ExtendedDocumentRoot extendedRoot = null;
		try {
			extendedRoot = (ExtendedDocumentRoot) extendedModel.getContents().get(0);
		} catch (Exception e) {
			fail(ROOT_DOCUMENT_ERROR);
		}
		assertNotNull(extendedRoot);
		
		TreeIterator<EObject> iterator = null;
		
		HashMap<String, EObject> bpmn2elements = new HashMap<String, EObject>();
		iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			
			//XML Informations in the document root will not be merged
			if (!(obj instanceof org.eclipse.emf.ecore.impl.EStringToStringMapEntryImpl)) {
				bpmn2elements.put(getStructuralFeature("id", obj), obj);
			}
		}
		
		iterator = extendedRoot.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			String id = getStructuralFeature("id", obj);
			if (bpmn2elements.containsKey(id)) {
				bpmn2elements.remove(id);
			}
		}

		assertEquals(0, bpmn2elements.size());
	}
	
	/**
	 * Tests the correct replacement of the Task and Lane Element.
	 * These elements should be extended to the ExtendedTask and 
	 * ExtendedLane objects.
	 */
	@Test
	public final void testMergeModels2() {
		Resource modelres = loadModel("trade.bpmn2");
		assertNotNull(modelres);
		
		Resource extensionModel = loadExtensionModel("trade.bpmn2extension");
		assertNotNull(extensionModel);
		
		Resource extendedModel = merger.run(modelres, extensionModel);
		
		ExtendedDocumentRoot extendedRoot = null;
		try {
			extendedRoot = (ExtendedDocumentRoot) extendedModel.getContents().get(0);
		} catch (Exception e) {
			fail(ROOT_DOCUMENT_ERROR);
		}
		assertNotNull(extendedRoot);
		
		boolean extensionError = false;
		
		TreeIterator<EObject> iterator = null;
		iterator = extendedRoot.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			
			if (obj instanceof org.eclipse.bpmn2.impl.TaskImpl) {
				if (!(obj instanceof carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl)) {
					extensionError = true;
					break;
				}
			} else if (obj instanceof org.eclipse.bpmn2.impl.LaneImpl 
					&& !(obj instanceof carisma.modeltype.bpmn2.extended.impl.ExtendedLaneImpl)) {
				extensionError = true;
				break;
			}
		}
		assertTrue(!extensionError);
	}
	
	/**
	 * Tests if the extended model contains the complete extension instance. 
	 * Subset Relationship
	 */
	@Test
	public final void testMergeModels3() {
		Resource modelres = loadModel("trade.bpmn2");
		assertNotNull(modelres);
		
		Resource extensionModel = loadExtensionModel("trade.bpmn2extension");
		assertNotNull(extensionModel);
		
		ExtensionRoot extensionRoot = null;
		try {
			extensionRoot = (ExtensionRoot) extensionModel.getContents().get(0);
		} catch (Exception e) {
			fail(ROOT_DOCUMENT_ERROR);
		}
		assertNotNull(extensionRoot);
		
		Resource extendedModel = merger.run(modelres, extensionModel);
		
		ExtendedDocumentRoot extendedRoot = null;
		try {
			extendedRoot = (ExtendedDocumentRoot) extendedModel.getContents().get(0);
		} catch (Exception e) {
			fail(ROOT_DOCUMENT_ERROR);
		}
		assertNotNull(extendedRoot);
		
		TreeIterator<EObject> iterator = null;
		
		HashMap<String, EObject> extensionElements = new HashMap<String, EObject>();
		iterator = extensionRoot.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			if (!(obj instanceof Task) && (obj instanceof Lane)) {
				extensionElements.put(getStructuralFeature("id", obj), obj);
			}
		}

		List<String> toBeRemoved = new ArrayList<String>();
		iterator = extendedRoot.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			String id = getStructuralFeature("id", obj);
			if (extensionElements.containsKey(id)) {
				toBeRemoved.add(id);
			}
		}

		for (String id : toBeRemoved) {
			extensionElements.remove(id);
		}
		assertEquals(0, extensionElements.size());
	}
}
