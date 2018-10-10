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
package carisma.ui.eclipse.rcp;

import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.ui.application.IWorkbenchConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
import org.eclipse.ui.ide.IDE;

import carisma.ui.eclipse.Perspective;


/**
 * 
 */
public class ApplicationWorkbenchAdvisor extends WorkbenchAdvisor {
	/**
	 * @param configurer IWorkbenchWindowConfigurer
	 * @return new ApplicationWorkbenchWindowAdvisor
	 */
	@Override
	public final WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
			final IWorkbenchWindowConfigurer configurer) {
		return new ApplicationWorkbenchWindowAdvisor(configurer);
	}
	/**
	 * @return Perspective.ID
	 */
	@Override
	public final String getInitialWindowPerspectiveId() {
		return Perspective.ID;
	}

	/**
	 * To get the resource workspace as input, override this method.
	 * @return the Root 
	 */
	@Override
	public final IAdaptable getDefaultPageInput() {
		return ResourcesPlugin.getWorkspace().getRoot();   
	}
	
	/**
	 * To get the correct adapters hooked up add this code to the initialize() method.
	 * @param configurer IWorkbenchConfigurer
	 */
	@Override
	public final void initialize(final IWorkbenchConfigurer configurer) {
		super.initialize(configurer);		                 
		IDE.registerAdapters();
	}

}
