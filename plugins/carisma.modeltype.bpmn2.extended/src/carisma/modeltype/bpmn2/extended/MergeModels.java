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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.bpmn2.ConversationLink;
import org.eclipse.bpmn2.DocumentRoot;
import org.eclipse.bpmn2.InteractionNode;
import org.eclipse.bpmn2.Lane;
import org.eclipse.bpmn2.LaneSet;
import org.eclipse.bpmn2.Process;
import org.eclipse.bpmn2.Task;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;

import carisma.modeltype.bpmn2.extended.impl.ExtendedTaskImpl;
import carisma.modeltype.bpmn2.extended.util.ExtendedResourceFactoryImpl;
import carisma.modeltype.bpmn2.extended.util.Tupel;
import carisma.modeltype.bpmn2.extension.ExtensionRoot;
import carisma.modeltype.bpmn2.extension.Role;
import carisma.modeltype.bpmn2.extension.WorkItem;


/**
 * This Class merges the extension bpmn2model with the standard bpmn2model
 * and creates an extended bpmn2model with additional informations.
 * @author Marcel Michel
 */
public class MergeModels {


	/**
	 * Method tries to load file extension model.
	 * If successful it delegates the merge operation to the merge method.
	 * 
	 * @param bpmn2Model The standard bpmn2 model
	 * @param extensionModel The extension model
	 * @return returns resource if the merge was successful otherwise null
	 */
	public static final Resource run(final Resource bpmn2Model, final Resource extensionModel) {
		
		if (bpmn2Model == null || extensionModel == null) {
			return null;
		}
		
		return merge(bpmn2Model, extensionModel);
	}

	
	/**
	 * Merges standard bpmn2model with bpmn2extension model and
	 * returns the merged bpmn2extendedmodel as a resource.
	 * @param stdModel The standard bpmn2 model
	 * @param extensionModel The extensionbmn2 model
	 * @return Returns merged bpmn2extended model as resource
	 */
	private static Resource merge(final Resource stdModel, final Resource extensionModel) {
		
		// Source bpmn2model
		DocumentRoot bpmn2ModelRoot = (DocumentRoot) stdModel.getContents().get(0);
		
		// Model with additional informations
		ExtensionRoot extensionRoot = (ExtensionRoot) extensionModel.getContents().get(0);
		
		// Merge the source model and the extension model in the extended model
		ExtendedFactory extendedFactory = ExtendedFactory.eINSTANCE;
		ExtendedDocumentRoot extendedRoot = extendedFactory.createExtendedDocumentRoot(); 
		
		// Copy first all content of the source model to the extended model
		extendedRoot.setExtendedDefinitions(EcoreUtil.copy(bpmn2ModelRoot.getDefinitions()));
		
		// Stored now in the extensionRoot
		extendedRoot.setExtensionRoot(EcoreUtil.copy(extensionRoot));
		
		// Creating resource, which later on be returned
		Resource extendedResource = new ExtendedResourceFactoryImpl().createResource(null);
		extendedResource.getContents().add(extendedRoot);	
		
		// Store replaced elements
		ArrayList<Tupel<Task, ExtendedTask>> extendedTasks =  
			new ArrayList<>();
		ArrayList<Tupel<Lane, ExtendedLane>> extendedLanes = 
			new ArrayList<>();
		
		// Need to update conversationLinks manually
		ArrayList<ConversationLink> conversationLinks =
				new ArrayList<>();
		
		TreeIterator<EObject> iterator = extendedRoot.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			
			// Search for elements, which should be extended
			if (obj instanceof Task) {
				ExtendedTask extTask = extendedFactory.createExtendedTask();
				extendedTasks.add(new Tupel<>((Task) obj, extTask));
			} else if (obj instanceof Lane) {
				ExtendedLane extLane = extendedFactory.createExtendedLane();
				extendedLanes.add(new Tupel<>((Lane) obj, extLane));
			} else if (obj instanceof ConversationLink) {
				conversationLinks.add((ConversationLink) obj);
			}
		}
		
		extendTaskElements(extendedRoot.getExtensionRoot(), extendedTasks, conversationLinks);
		extendLaneElements(extendedRoot.getExtensionRoot(), extendedLanes);
		
		/* Debug */
		//printContent(extRoot);
		
		return extendedResource;
	}
	
	/**
	 * Extends a standard bpmn2 lane element with additional information.
	 * @param extensionRoot Includes the additional information
	 * @param extLanes The lane objects, which should be extended
	 */
	private static void extendLaneElements(ExtensionRoot extensionRoot,
			ArrayList<Tupel<Lane, ExtendedLane>> extLanes) {
		// Creating HashMap to map elements name to extension information
		Map<String, carisma.modeltype.bpmn2.extension.Lane> extensionMapLane = 
			createHashMap(extensionRoot.getLane());
		
		// Replace elements with extended elements
		// Until now they still do not save additional information
		for (Tupel<Lane, ExtendedLane> t  : extLanes) {
			copyFeatures(t.getO1(), t.getO2());
			((LaneSet) t.getO1().eContainer()).getLanes().add(t.getO2());
			EcoreUtil.remove(t.getO1());
			
			// If available add additional information to each replaced element
			if (extensionMapLane.containsKey(t.getO2().getName().toLowerCase(Locale.ENGLISH))) {
				carisma.modeltype.bpmn2.extension.Lane extensionLane = 
					extensionMapLane.get(t.getO2().getName().toLowerCase(Locale.ENGLISH));
				extensionLane.setId(t.getO2().getId());
				for (Role r : extensionLane.getRole()) {
					t.getO2().getRole().add(r);
				}
			}
		}
	}


	/**
	 * Extends a standard bpmn2 Task element with additional information.
	 * @param extensionRoot Includes the additional information
	 * @param extTasks The task objects, which should be extended
	 * @param conversationLinks The conversation link objects must be manually updated
	 */
	private static void extendTaskElements(ExtensionRoot extensionRoot, 
			List<Tupel<Task, ExtendedTask>> extTasks, List<ConversationLink> conversationLinks) {
		// Creating HashMap to map elements name to extension information
		Map<String, carisma.modeltype.bpmn2.extension.Task> extensionMapTask = 
				createHashMap(extensionRoot.getTask());
			
		for (Tupel<Task, ExtendedTask> t  : extTasks) {
			
			// Replace elements with extended elements
			// Until now they still do not save additional information
			copyFeatures(t.getO1(), t.getO2());
			((Process) t.getO1().eContainer()).getFlowElements().add(t.getO2());
			EcoreUtil.remove(t.getO1());
			
			// If available add additional information to each replaced element
			if (extensionMapTask.containsKey(t.getO2().getName().toLowerCase(Locale.ENGLISH))) {
				carisma.modeltype.bpmn2.extension.Task extensionTask = 
					extensionMapTask.get(t.getO2().getName().toLowerCase(Locale.ENGLISH));
				extensionTask.setId(t.getO2().getId());
				for (WorkItem w : extensionTask.getWorkItem()) {
					t.getO2().getWorkItem().add(w);
				}
			}
		}
		
		// ConversationLink Object must be updated manually
		InteractionNode tmp;
		for (ConversationLink con : conversationLinks) {
			for (Tupel<Task, ExtendedTask> t : extTasks) {
				if (t.getO1().equals(con.getSourceRef())) {
					con.setSourceRef(t.getO2());
				}
				if (t.getO1().equals(con.getTargetRef())) {
					con.setTargetRef(t.getO2());
				}
			}
			// Update ConversationLink References in the ExtendedTask Element
			tmp = con.getSourceRef();
			if (tmp != null && tmp instanceof ExtendedTaskImpl) {
				List<ConversationLink> list = ((ExtendedTaskImpl) tmp).getOutgoingConversationLinks();
				list.add(con);
				((ExtendedTaskImpl) tmp).setOutgoingConversationLinks(list);
			}
			tmp = con.getTargetRef();
			if (tmp != null && tmp instanceof ExtendedTaskImpl) {
				List<ConversationLink> list = ((ExtendedTaskImpl) tmp).getIncomingConversationLinks();
				list.add(con);
				((ExtendedTaskImpl) tmp).setIncomingConversationLinks(list);
			}
		}
	}


	/**
	 * Creates Map to map object name to the object itself.
	 * Necessary type of the object: bpmn2extension.Process or 
	 * bpmn2extension.Task or bpmn2extension.Lane
	 * @param <T> The type of the object
	 * @param list The Objects which should be mapped
	 * @return Map of the Objects
	 */
	private static <T> Map<String, T> createHashMap(final EList<T> list) {
		Map<String, T> map = new HashMap<>();
		for (T elem : list) {
			if (elem instanceof carisma.modeltype.bpmn2.extension.Task) {
				map.put(((carisma.modeltype.bpmn2.extension.Task) elem).getName().toLowerCase(Locale.ENGLISH), elem);
			} else if (elem instanceof carisma.modeltype.bpmn2.extension.Lane) {
				map.put(((carisma.modeltype.bpmn2.extension.Lane) elem).getName().toLowerCase(Locale.ENGLISH), elem);
			} 
		}
		return map;
	}
	
	/**
	 * Called to copy EStructuralFeatures from obj1 to obj2.
	 * @param obj1 The object from which to copy
	 * @param obj2 The object to copy to
	 */
	private static void copyFeatures(final EObject obj1, final EObject obj2) {
		for (EStructuralFeature feature : obj1.eClass().getEAllStructuralFeatures()) {
			
			// Case Task: incoming and outgoing conversation links cannot be copied
			// cause of a virtual adapter, which is only implemented for bpmn2 resources
			if (!feature.getName().equals("incomingConversationLinks")
					&& !feature.getName().equals("outgoingConversationLinks")) {
				obj2.eSet(feature, obj1.eGet(feature));
			}
		}
	}
	
	/**
	 * Prints eContents to console.
	 * @param root The rootObject
	 */
	@SuppressWarnings("unused")
	private static void printContent(final EObject root) {
		TreeIterator<EObject> iterator = root.eAllContents();
		while (iterator.hasNext()) {
			EObject obj = iterator.next();
			System.out.println(obj.toString());
		}
	}
}
