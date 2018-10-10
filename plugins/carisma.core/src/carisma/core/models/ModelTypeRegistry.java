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
package carisma.core.models;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.URIConverter;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


/**
 * Manages the types of supported models.
 * @author wenzel
 *
 */
public class ModelTypeRegistry {

	private List<ModelType> registeredTypes;
	
	public ModelTypeRegistry() {
		this.registeredTypes = new ArrayList<>();
	}

	/**
	 * Loads the available model types from the Eclipse extension point registry.
	 */
	public void initialize() {
		// search for adapters in ExtensionRegistry
		IExtensionRegistry extensionRegistry = Platform.getExtensionRegistry();
		IExtensionPoint extensionPoint = extensionRegistry.getExtensionPoint(Carisma.EXTENSION_POINT_CARISMAMODELTYPE);
		for (IExtension extension : extensionPoint.getExtensions()) {
			for (IConfigurationElement pluginElement : extension.getConfigurationElements()) {
				String name = pluginElement.getAttribute("name");
				String nsURI = pluginElement.getAttribute("nsURI");
				String fileExtension = pluginElement.getAttribute("fileExtension");
				ModelType modelType = new ModelType(name, nsURI, fileExtension, pluginElement);
				this.registeredTypes.add(modelType);
			}
		}
	}
	
	public boolean isSupportedType(String modelType) {
		for (ModelType mt : this.registeredTypes) {
			if (mt.getName().equalsIgnoreCase(modelType)) {
				return true;
			}
		}
		return false;
	}
	
	@Deprecated
	public ModelLoader getLoader(String modelType) {
		for (ModelType mt : this.registeredTypes) {
			if (mt.getName().equalsIgnoreCase(modelType)) {
				return mt.instantiateLoader();
			}
		}
		Logger.log(LogLevel.WARNING, "No model loader found for model type '" + modelType + "'");
		return null;
	}
	
	public static void registerURIMapping(String uri, String mapping) {
		URI modelUri = URI.createURI(uri);
		if (!URIConverter.URI_MAP.containsKey(modelUri)) {
			URIConverter.URI_MAP.put(modelUri, URI.createURI(mapping));
		}
	}
	
	public List<ModelType> getSupportedTypes() {
		return this.registeredTypes;
	}

	public ModelType getTypeForExtension(String fileExtension) {
		for (ModelType mt : this.registeredTypes) {
			String[] modelTypes = mt.getFileExtension().split(",");
			for (String modelExtension : modelTypes) {
				if (modelExtension.equalsIgnoreCase(fileExtension)) {
					return mt;
				}
			}
		}
		return null;
	}
	
	public ModelType getModelTypeWithName(String typeName) {
		for (ModelType mt : this.registeredTypes) {
			if (mt.getName().equalsIgnoreCase(typeName)) {
				return mt;
			}
		}
		return null;
	}
	
}
