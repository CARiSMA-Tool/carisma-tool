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
package carisma.modeltype.bpmn2;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.HashMap;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelLoader;
import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;

/**
 * BPMN2 ModelLoader.
 * @author Marcel Michel
 * 
 */
@Deprecated
public class BPMN2ModelLoader implements ModelLoader {

	/**
	 * Loads instances of BPMN2 models.
	 * @param file the BPMN2 model-file
	 * @exception IOException IOException
	 * @return the new model
	 */
	@Override
	public final Resource load(final File file) throws IOException {
		// Model must have the prefix bpmn2. See Ticket #1175 for further info.
		File newFile = File.createTempFile(".tmp", ".bpmn2", file.getParentFile());
		//FIX DP: To write with an UTF-8 encoding you need to explicitly state that
		try (OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(newFile),"UTF-8")){

			if (YaoqiangHelper.isYaoqiangModel(file.getAbsolutePath())) {
				String content = YaoqiangHelper.yaoqiang2emfModel(file.getAbsolutePath());
				writer.write(content);
				writer.flush();
				writer.close();
				/* Nice to know: 
				 * resource.load(new URIConverter.ReadableInputStream(StringContent), null); */
			} else {
				try(BufferedReader reader = new BufferedReader(new FileReader(file))){
					String line = reader.readLine();
					while (line != null) {
						writer.write(line);
						line = reader.readLine();
					}
					writer.flush();
					writer.close();
					reader.close();
				}
			}
		}

		ResourceSet resourceSet = new ResourceSetImpl();
		URI uri = URI.createFileURI(newFile.getAbsolutePath());
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		
		// Set the URI to the default model path
		// This is important for writeBack 
		uri = URI.createFileURI(file.getAbsolutePath());
		resource.setURI(uri);
		
		if (!newFile.delete()) {
		    Logger.log(LogLevel.WARNING, "Failed to delete '" + newFile.getName());
		}
		
		return resource;
	}
}
