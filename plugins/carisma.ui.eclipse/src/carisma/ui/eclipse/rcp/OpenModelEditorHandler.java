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

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;


/**
 *
 */
public class OpenModelEditorHandler extends OpenModelHandler {
	
	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		
		return super.execute(false);
	}
	
	@Override
	protected final IFile getSelectedFile() { 
		return HandlerUtilz.getSelectedEditorFile();
	}
	
	@Override
	public final boolean isEnabled() {
		IFile selectedFile = getSelectedFile();
		setBaseEnabled(false);
		if (selectedFile != null) {
			Analysis analysis = AnalysisUtil.readAnalysis(selectedFile.getLocation().toOSString());
			if (!(analysis.getSelectedEditorId() == null || "".equals(analysis.getSelectedEditorId()))) {
				return true;
			}
		}
		return false;
	}
}
