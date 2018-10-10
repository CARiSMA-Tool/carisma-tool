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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/**
 * Describes a model type that is supported by CARiSMA.
 * @author wenzel
 *
 */
public class ModelType {

	private String name;
	private String nsURI;
	private IConfigurationElement extensionPointConfigurationElement;
	private String fileExtension;
	
	public ModelType(String name, String nsURI, String fileExtension, IConfigurationElement loaderClazz) {
		super();
		this.name = name;
		this.nsURI = nsURI;
		this.extensionPointConfigurationElement = loaderClazz;
		this.fileExtension = fileExtension;
	}

	public String getName() {
		return this.name;
	}

	public String getNsURI() {
		return this.nsURI;
	}

	@Deprecated
	public ModelLoader instantiateLoader() {
		try {
			return (ModelLoader) this.extensionPointConfigurationElement.createExecutableExtension("loader");
		} catch (CoreException e) {
			// TODO Fehlermeldung ausgeben
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		return null;
	}

	public String getFileExtension() {
		return this.fileExtension;
	}
	
}
