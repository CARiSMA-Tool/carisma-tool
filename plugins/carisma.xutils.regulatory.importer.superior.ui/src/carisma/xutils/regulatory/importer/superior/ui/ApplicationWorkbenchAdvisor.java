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

import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchAdvisor;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

/**
 * ApplicationWorkbenchAdvisor.
 * @author jkowald
 */
public class ApplicationWorkbenchAdvisor extends WorkbenchAdvisor {

	/**
	 * The perspective id.
	 */
	private static final String PERSPECTIVE_ID = "carisma.xutils.regulatory.importer.superior.ui.perspective";

	@Override
	public final WorkbenchWindowAdvisor createWorkbenchWindowAdvisor(
			final IWorkbenchWindowConfigurer configurer) {
		return new ApplicationWorkbenchWindowAdvisor(configurer);
	}

	@Override
	public final String getInitialWindowPerspectiveId() {
		return PERSPECTIVE_ID;
	}

}
