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

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import carisma.ui.eclipse.popup.actions.RunAnalysisAction;



/**
 * Handler for command execution by CARiSMA menu item.
 */
public class RunAnalysisEditorHandler extends AbstractHandler {
	/**
	 * 
	 */
	private RunAnalysisAction runAction = null;
	/**
	 * Constructor.
	 */
	public RunAnalysisEditorHandler() {
		this.runAction = new RunAnalysisAction();
	}
	
	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		if (this.runAction.initSelectionByEditor()) {
			this.runAction.run(null);
		}
		
		return null;
		
	}
	
}
