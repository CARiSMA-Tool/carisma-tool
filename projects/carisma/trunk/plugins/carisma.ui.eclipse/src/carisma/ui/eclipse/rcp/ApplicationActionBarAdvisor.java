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

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
/**
 * 
 */

//TODO: What is the purpose of this class?
public class ApplicationActionBarAdvisor extends ActionBarAdvisor {
	/**
	 * 
	 * @param configurer IActionBarConfigurer
	 */
	public ApplicationActionBarAdvisor(final IActionBarConfigurer configurer) {
		super(configurer);
	}

//	protected void makeActions(IWorkbenchWindow window) {
//		super.makeActions(window);
//		aboutAction = ActionFactory.ABOUT.create(window);
//		register(aboutAction);
//	}
	/**
	 * @param menuBar IMenuManager
	 */
	@Override
	protected void fillMenuBar(final IMenuManager menuBar) {
		
	}
}
