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
package carisma.ui.eclipse.editors;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;

/**
 * Manages the types of supported editors.
 */
public class EditorRegistry {

	/**
	 * List of registered editors.
	 */
	private List<EditorDescriptor> registeredEditors;
	
	//########################################################################################
	/**
	 * Constructor.
	 */
	public EditorRegistry() {
		this.registeredEditors = new ArrayList<>();
	}
	
	/**
	 * Register editor descriptors.
	 */
	public final void initialize() {
		// search for adapters in ExtensionRegistry
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(CarismaGUI.EDITOR_DESCRIPTION);
		
		for (IExtension extension : extensionPoint.getExtensions()) {
			for (IConfigurationElement checkElement : extension.getConfigurationElements()) {
				try {
					EditorDescriptor editorDescrtipion = (EditorDescriptor) checkElement.createExecutableExtension("implementingClass");
					editorDescrtipion.setID(checkElement.getAttribute("id"));
					editorDescrtipion.setName(checkElement.getAttribute("name"));
					List<String> types = new ArrayList<>();
					for (IConfigurationElement parameterElement : checkElement.getChildren("EditorType")) {
						types.add(parameterElement.getAttribute("Name")); 
					}
					editorDescrtipion.setExtension(checkElement.getAttribute("file_extension"));
					editorDescrtipion.setTypes(types);
					this.registeredEditors.add(editorDescrtipion);
				} catch (CoreException e) {
					Logger.log(LogLevel.DEBUG, "Unable to load check class: " + checkElement.getAttribute("id"), e);
				}
			}
		}
	}
	
	/**
	 * Getter for the list of registered editors.
	 * @return registeredEditors
	 */
	public final List<EditorDescriptor> getRegisteredEditors() {
		return this.registeredEditors;
	}
	
	/**
	 * Getter for EditorDescriptors identified by their name.
	 * @param name the Name of the EditorDescriptor to search for
	 * @return <code>EditorDescriptor</code> for this name or <code>null</code> when EditorDescriptor not found
	 */
	public final EditorDescriptor getEditorDescriptorByName(final String name) {
		for (EditorDescriptor ed : this.registeredEditors) {
			if (ed.getName().equalsIgnoreCase(name)) {
				return ed;
			}
		}
		return null;
	}
	
	/**
	 *  Getter for EditorDescriptors identified by their id.
	 * @param id the Id of of the EditorDescriptor to search for
	 * @return <code>EditorDescriptor</code> for this Id or <code>null</code> when EditorDescriptor not found 
	 */
	public final EditorDescriptor getEditorDescriptorById(final String id) {
		for (EditorDescriptor ed : this.registeredEditors) {
			if (ed.getId().equalsIgnoreCase(id)) {
				return ed;
			}
		}
		return null;
	}
}
