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
	    
	    
	    IFolderLayout leftTopFolder = layout.createFolder("LeftTop", IPageLayout.LEFT, 0.2f, editorArea);
        leftTopFolder.addView(IPageLayout.ID_PROJECT_EXPLORER);

        IFolderLayout leftBottomFolder = layout.createFolder("LeftBottom", IPageLayout.BOTTOM, 0.5f, "LeftTop");
        leftBottomFolder.addView("org.eclipse.papyrus.views.modelexplorer.modelexplorer");

	    

	    IFolderLayout bottomfolder = layout.createFolder("Bottom", IPageLayout.BOTTOM, 0.7f, editorArea);
	    bottomfolder.addView(IPageLayout.ID_PROP_SHEET);
	    bottomfolder.addView(AnalysisResultsView.ID);
	 
	}
}
	