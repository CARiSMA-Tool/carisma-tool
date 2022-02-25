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
package carisma.profile.umlsec;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Collections;

import org.eclipse.core.runtime.Plugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.uml2.uml.Profile;
import org.eclipse.uml2.uml.resource.UMLResource;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


public class UMLsecActivator extends Plugin {

	public static final String PLUGIN_ID = "carisma.profile.umlsec";

	public static final String EMF_URI = "http://www.umlsec.de/profiles/UMLsec";
	public static final String EMF_FILE = "platform:/plugin/carisma.profile.umlsec/profile/UMLsec.ecore";

	public static final String UML_URI = "http://www.umlsec.de/profiles/UMLsec";
	public static final String UML_FILE = "platform:/plugin/carisma.profile.umlsec/profile/UMLsec.profile.uml";

	@Override
	public void start(final BundleContext context) throws Exception {
		try {
			Carisma.getInstance().getModelManager().addMapping("UMLsec.profile.uml", UMLsecActivator.UML_FILE);
		} catch (final Exception e) {
			Logger.log(LogLevel.ERROR, "Error while putting profile path to map", e);
		}
	}

	/**
	 * Loads the UMLsec profile as resource in the given resource set
	 *
	 * @param rs the resource set
	 * @throws IOException If the profile cannot be loaded
	 */
	public static Profile loadUMLsecProfile(final ResourceSet rs) throws IOException {
		rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put("uml", UMLResource.Factory.INSTANCE);
		final Resource umlSecPackage = rs.createResource(URI.createURI(UML_URI));
		URL resource = UMLsecActivator.class.getResource("/profile/UMLsec.profile.uml");
		if(resource == null) {
			resource = UMLsecActivator.class.getResource("/UMLsec.profile.uml");
		}
		try(InputStream stream = resource.openStream()){
			umlSecPackage.load(stream, Collections.EMPTY_MAP);
		}
		return (Profile) umlSecPackage.getContents().get(0);
	}

}
