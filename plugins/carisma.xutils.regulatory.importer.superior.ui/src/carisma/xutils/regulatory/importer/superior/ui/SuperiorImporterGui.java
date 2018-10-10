/*******************************************************************************
 * Copyright (c) 2012 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.xutils.regulatory.importer.superior.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 * @author jkowald
 */
public class SuperiorImporterGui extends AbstractUIPlugin {

	/**
	 * The plug-in ID.
	 */
	public static final String PLUGIN_ID = "carisma.xutils.regulatory.importer.superior.ui"; //$NON-NLS-1$

	/**
	 * The shared instance.
	 */
	private static SuperiorImporterGui instance;
	
	/**
	 * The constructor.
	 */
	public SuperiorImporterGui() {
	}

	@Override
	public final void start(final BundleContext context) throws Exception {
		super.start(context);
		instance = this;
	}

	@Override
	public final void stop(final BundleContext context) throws Exception {
		instance = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 * @return the shared instance
	 */
	public static SuperiorImporterGui getDefault() {
		return instance;
	}

	/**
	 * Returns an image descriptor for the image file at the given
	 * plug-in relative path.
	 * @param path the path
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(final String path) {
		return imageDescriptorFromPlugin(PLUGIN_ID, path);
	}
}
