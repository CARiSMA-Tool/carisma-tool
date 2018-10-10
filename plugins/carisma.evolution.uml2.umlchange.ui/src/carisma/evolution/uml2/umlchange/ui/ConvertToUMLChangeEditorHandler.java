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

package carisma.evolution.uml2.umlchange.ui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

/**
 * A handler class, needed when a resource was selected in Editor.
 */
public class ConvertToUMLChangeEditorHandler extends AbstractHandler {
	/**
	 * 
	 */
	private ConvertToUMLChangeAnalysis convertAction = null;
	
	/**
	 * Constructor.
	 */
	public ConvertToUMLChangeEditorHandler() {
		this.convertAction = new ConvertToUMLChangeAnalysis();
	}
	
	
	@Override
	public final Object execute(final ExecutionEvent event)
			throws ExecutionException {
		
		if (this.convertAction.initSelectionByEditor()) {
			this.convertAction.run(null);
		}

		return null;
	}
}
