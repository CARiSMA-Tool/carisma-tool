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
package carisma.ui.eclipse;

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import carisma.ui.eclipse.views.AnalysisResultsView;
//import carisma.xutils.regulatory.importer.superior.ui.LogView;
//import carisma.xutils.regulatory.importer.superior.ui.SettingsView;

/**
 * 
 */
public class Perspective implements IPerspectiveFactory {
	/**
	 * 
	 */
	public static final String ID = "carisma.perspective";
	
	/**
	 * @param layout the page layout
	 */
	@Override
	public final void createInitialLayout(final IPageLayout layout) {
		String editorArea = layout.getEditorArea();
		layout.setEditorAreaVisible(true);

		layout.addStandaloneView("org.eclipse.ui.views.navigator.ResourceNavigator", true, IPageLayout.LEFT, 0.3f, editorArea);
		
		IFolderLayout bottomfolder = layout.createFolder("Bottom",
				IPageLayout.BOTTOM, 0.7f, editorArea);
		bottomfolder.addView("org.eclipse.pde.runtime.LogView");
		bottomfolder.addView(AnalysisResultsView.ID);
	}

}
