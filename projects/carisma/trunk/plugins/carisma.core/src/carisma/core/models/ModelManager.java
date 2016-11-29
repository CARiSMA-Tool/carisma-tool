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

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.ecore.resource.Resource;

import carisma.core.Carisma;


/**
 * Manages the models to be analyzed.
 * @author wenzel
 *
 */
public class ModelManager {
	
	/**
	 * HashMap where the correct profile-paths should be mapped.
	 */
	private HashMap<String, String> profileMapping = new HashMap<>();

	private File workdir = null;
	
	public void setWorkingDirectory(File wd) {
		this.workdir = wd;
	}
	
	public File getFile(IFile iFile) {
		File file = null;
		if (iFile.getLocation()!=null) {
			file = iFile.getLocation().toFile();
		} else {
			String path = this.workdir.getAbsolutePath();
			if (path.endsWith("/")) {
				path = path.substring(0, path.length()-2);
			}
			path += iFile.getFullPath();
			file = new File(path);
		}
		return file;
	}
	
	/**
	 * Loads a model for analysis and returns it. 
	 * @param file The file from which the model is to be loaded
	 * @param modelType The type of the model, which refers to the model loader (i.e. as defined in the plugin.xml of the modeltype plugin.
	 * @return
	 * @throws IOException
	 */
	@Deprecated
	public static Resource loadModel(File file, String modelType) throws IOException {
		ModelLoader loader = Carisma.getInstance().getModelTypeRegistry().getLoader(modelType);
		if (loader != null) {
			return loader.load(file);
		}
		throw new RuntimeException("No loader for model type " + modelType);
	}
	
	/**
	 * Adds a new mapping to the profileMapping HashMap.
	 * @param key new key
	 * @param value new Value
	 */
	public final void addMapping(final String key, final String value) {
		this.profileMapping.put(key, value);
	}
	
	public final Map<String, String> getMapping() {
		return Collections.unmodifiableMap(this.profileMapping);

	}
	
	/**
	 * Use loadModel instead!
	 */
	@Deprecated 
	public static Resource loadModelSeperately(File file, String modelType) throws IOException {
		return loadModel(file, modelType);
	}

}
