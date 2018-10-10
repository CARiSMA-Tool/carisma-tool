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

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.graphics.Point;
import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;
/**
 * 
 */
public class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {
	/**
	 * @param configurer IWorkbenchWindowConfigurer
	 */
	public ApplicationWorkbenchWindowAdvisor(
			final IWorkbenchWindowConfigurer configurer) {
		super(configurer);
	}
	/**
	 * @param configurer IActionBarConfigurer
	 * @return new ApplicationActionBarAdvisor
	 */
	@Override
	public final ActionBarAdvisor createActionBarAdvisor(
			final IActionBarConfigurer configurer) {
		return new ApplicationActionBarAdvisor(configurer);
	}
	/**
	 * 
	 */
	@Override
	public final void preWindowOpen() {
		IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
		configurer.setInitialSize(new Point(1024, 768));
		configurer.setShowCoolBar(false);						// Toolbar hidden
		configurer.setShowStatusLine(true);
		configurer.setTitle("CARiSMA");
		
	}

	@Override
	public final void postWindowOpen() {
		
//	remove unused menus
		IMenuManager mm =
	            getWindowConfigurer().getActionBarConfigurer().getMenuManager();
	        IContributionItem[] mItems = mm.getItems();
	        for (int i = 0; i < mItems.length; i++) {
	            
	            if (mItems[i].getId().equals("navigate") 
	            		|| mItems[i].getId().equals("additions") 
	            		|| mItems[i].getId().equals("org.eclipse.search.menu") 
	            		|| mItems[i].getId().equals("org.eclipse.ui.run") 
	            		|| mItems[i].getId().equals("org.eclipse.jdt.ui.refactoring.menu") 
	            		|| mItems[i].getId().equals("org.eclipse.jdt.ui.source.menu")) {
	              mm.remove(mItems[i].getId());
	              mm.update(true);
	            }
	        }
	}
}
