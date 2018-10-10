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

import org.eclipse.ui.IFolderLayout;
import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

/**
 * The definition of the superior importer UI perspective. Only used
 * when the UI is started as a standalone application.
 * @author jkowald
 */
public class Perspective implements IPerspectiveFactory {

	@Override
	public final void createInitialLayout(final IPageLayout layout) {
		layout.setEditorAreaVisible(false);
		layout.setFixed(true);
		
		IFolderLayout folderLayout = layout.createFolder("whole", IPageLayout.LEFT, 0.95f, "");
		
		folderLayout.addView("carisma.xutils.regulatory.importer.superior.ui.SettingsView");
		folderLayout.addView("carisma.xutils.regulatory.importer.superior.ui.LogView");
	}

}
