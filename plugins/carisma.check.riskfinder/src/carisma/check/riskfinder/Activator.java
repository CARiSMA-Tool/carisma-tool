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
package carisma.check.riskfinder;


import java.util.Observer;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;
import de.fraunhofer.isst.wbmp.model.*;

/**
 * The activator class controls the plug-in life cycle.
 */
public class Activator extends AbstractUIPlugin {

	/** The plug-in ID.
	 * 
	 **/
	public static final String PLUGIN_ID = "carisma.check.riskfinder"; //$NON-NLS-1$

	/** The shared instance. 
	 **/
	private static Activator plugin;
	
	/**
	 * the riskfinderGui instance.
	 */
	private static Observer riskfinderGui;
	
	/** .*/
	public static Document patternDocument = null;
	
	/** .*/
	public static Document processDocument = null;
	
	/**
	 * The constructor.
	 **/
	public Activator() {
	}

	/**
	 * (non-Javadoc) (jetzt schon!).
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 * @param context The context. //TODO
	 * @throws Exception ??? //TODO
	 **/
	public final void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/**
	 * (non-Javadoc).
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 * @param context The context. //TODO
	 * @throws Exception ??? //TODO
	 **/
	public final void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}
	
	/**
	 * registers the riskfinderGui as Observer.
	 * @param obs the riskfinderGui
	 */
	public final void registerGuiObserver(final Observer obs) {
		riskfinderGui = obs;
	}
	
	/**
	 * returns the risfinderGUI.
	 * @return the riskfinderGUI, null if the GUI isn't registered yet
	 */
	public final Observer getRiskfinderGui() {
		return riskfinderGui;
	}

}
