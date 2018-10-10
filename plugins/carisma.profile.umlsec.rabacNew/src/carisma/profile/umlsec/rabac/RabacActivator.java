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
package carisma.profile.umlsec.rabac;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class RabacActivator extends Plugin {

	public static final String EMF_URI = "http://www.umlsec.de/profiles/UMLsec/RABAC";
	public static final String EMF_FILE = "platform:/plugin/carisma.profile.umlsec.rabac/profile/RABAC.ecore";
	
	public static final String UML_URI = "http://www.umlsec.de/profiles/UMLsec/RABAC";
	public static final String UML_FILE = "platform:/plugin/carisma.profile.umlsec.rabac/profile/RABAC.profile.uml";
	
	@Override
	public void start(BundleContext context) throws Exception {
		try {
			Carisma.getInstance()
					.getModelManager()
					.addMapping("RABAC.profile.uml",
							RabacActivator.UML_FILE);
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Error while putting profile path to map", e);
		}
	}

}
