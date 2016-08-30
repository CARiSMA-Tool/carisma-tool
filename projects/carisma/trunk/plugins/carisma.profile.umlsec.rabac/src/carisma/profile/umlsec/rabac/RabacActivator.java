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

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class RabacActivator implements BundleActivator {

	public static final String RABAC_PROFILE_FOLDER = "platform:/plugin/carisma.profile.umlsec.rabac/profile/";

	@Override
	public void start(BundleContext context) throws Exception {
		try {
			Carisma.getInstance()
					.getModelManager()
					.addMapping("RABAC.profile.uml",
							"platform:/plugin/carisma.profile.umlsec.rabac/profile/RABAC.profile.uml");
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Error while putting profile path to map", e);
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub

	}

}
