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
package carisma.modeltype.uml2;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.uml2.uml.Model;
import org.eclipse.uml2.uml.NamedElement;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.models.ModelLoader;


@Deprecated
public class UML2ModelLoader implements ModelLoader {

	/**
	 * Loads instances of UML2 models and returns a manipulated model with new profile paths (UMLsec, UMLchange).
	 * @param file the UML" model-file
	 * @exception IOException IOException
	 * @return the new model
	 */
	@Override
	@Deprecated
	public final Resource load(final File file) throws IOException {
		
		Map<String, String> profileLocationMapping;
		File newFile = File.createTempFile(".tmp", ".uml", file.getParentFile());
		ResourceSet resourceSet = null;
		try (
				BufferedReader reader = new BufferedReader(new FileReader(file));
				BufferedWriter writer = new BufferedWriter(new FileWriter(newFile)))
		{
    		String lineToCheck;
    		profileLocationMapping = Carisma.getInstance().getModelManager().getMapping();
    		resourceSet = new ResourceSetImpl();
    		lineToCheck = reader.readLine();
    		while (lineToCheck != null) {
    			for (Entry<String, String> profileLocationEntry : profileLocationMapping.entrySet()) {
    				String profileName = profileLocationEntry.getKey();
    				if (lineToCheck.contains(profileLocationEntry.getKey())) {
    					String profileUri = profileLocationEntry.getValue();
    //					System.out.println("Alte Zeile " + count + (": " + lineToCheck));
    					Pattern xmiPattern = Pattern.compile("(<xmi:.*) [^\\ ]*" + profileName.replace("\\.", "\\\\.") + "(.*)");
    					Matcher xmiMatcher = xmiPattern.matcher(lineToCheck);
    					
    					if (xmiMatcher.find()) {
    						lineToCheck = xmiMatcher.replaceFirst("$1 " + profileUri + "$2");
    					} else {
    						Pattern hrefPattern = Pattern.compile("(.*)href[^\\ ]*" + profileName.replace("\\.", "\\\\.") + "(.*)");
    						Matcher hrefMatcher = hrefPattern.matcher(lineToCheck);
    						lineToCheck = hrefMatcher.replaceFirst("$1" + "href=\"" + profileUri + "$2");
    					}
    //					System.out.println("Neue Zeile " + count + (": " + lineToCheck));
    				}
				}
    			writer.write(lineToCheck);
    			writer.newLine();
    			lineToCheck = reader.readLine();
    		}
            writer.flush();
		}
		
		URI uri = URI.createFileURI(newFile.getAbsolutePath());
		Resource resource = resourceSet.getResource(uri, true);
		resource.load(new HashMap<String, Object>());
		String defaultName = "defaultModelName";
		for (EObject object : resource.getContents()) {
			if ((object instanceof Model)
					&& ((((NamedElement) object).getName() == null) || (((NamedElement) object).getName().equals("")))) {
				((Model) object).setName(defaultName);
				Logger.log(LogLevel.INFO, "The model element got no name, name was set to \"" + defaultName + "\".");
			}
		}
		if (!newFile.delete()) {
		    Logger.log(LogLevel.WARNING, "Failed to delete '" + newFile.getName());
		}
		return resource;
	}
}
