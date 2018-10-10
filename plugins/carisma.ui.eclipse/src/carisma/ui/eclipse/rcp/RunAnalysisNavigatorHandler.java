package carisma.ui.eclipse.rcp;
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
import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.ui.eclipse.popup.actions.RunAnalysisAction;


/**
 *
 */
public class RunAnalysisNavigatorHandler extends AbstractHandler {

	/**
	 * 
	 */
	private RunAnalysisAction runAction = null;
	/**
	 * Constructor.
	 */
	public RunAnalysisNavigatorHandler() {
		this.runAction = new RunAnalysisAction();
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {

		if (this.runAction.initSelectionByService()) {
			this.runAction.run(null);
		}
		
		return null;
	}
	
	
	@Override
	public final boolean isEnabled() {
		IFile selectedFile = HandlerUtilz.getSelectedNavigatorFile();
		setBaseEnabled(false);
		if (selectedFile != null) {
			Analysis analysis = AnalysisUtil.readAnalysis(selectedFile.getLocation().toOSString());
			if (!(analysis.getChecks() == null || analysis.getChecks().size() == 0)) {
				return true;
			}
		}
		return false;
	}
}
