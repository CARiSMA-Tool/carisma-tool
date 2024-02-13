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
import org.eclipse.core.resources.IFile;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.ui.eclipse.editors.EditorTranslator;


/**
 *
 */
public abstract class OpenModelHandler extends AbstractHandler {

 	/**
	 * @param openwith true, when the command "open with" is called, 
	 * false - when command "open" is called
	 * @return null
	 */
	public final Object execute(final boolean openwith) {
		/**
		 * is the model file of the selected file. 
		 */
		IFile modelFile = getModelFile(); 
		/**
		 * the selected resource
		 */
		IFile selectedFile = getSelectedFile();
		/**
		 * the analysis.
		 */	
		if (selectedFile == null) {
		    //TODO
		    return null;
		}
		Analysis analysis = AnalysisUtil.readAnalysis(selectedFile.getLocation().toOSString());
		
	    if (analysis == null || modelFile == null || !modelFile.exists()) {
	    	return null;
	    }
	    EditorTranslator editorTranslator = new EditorTranslator(analysis);
	    editorTranslator.openEditor(false);
	    
	    /* TODO (jk) Check if the following is necessary without select editor dialog
	    et.handleManually(analysis.getModelFile().toString(), openwith);

	    if (et.getRemember() && !"".equals(et.getSaveSelectionEditorId())) { // save decision
			analysis.setSelectedEditorId(et.getSaveSelectionEditorId());
			
			Display display = Display.getDefault();
		    Shell shell = new Shell(display);
			MessageDialog mDialog = new MessageDialog(shell,
					"Save and Launch", null, 
					"Do you want to save the changes in" + selectedFile
					.getLocation().toOSString() + "?", 0, 
					new String[]{"OK", "Cancel"}, 0);
			mDialog.open();
			
			if (mDialog.getReturnCode() == 0) {  		// save analysis 	
				AnalysisUtil.storeAnalysis(analysis, selectedFile.getLocation().toOSString());
				// refresh resource
				try {
					selectedFile.refreshLocal(IResource.DEPTH_ZERO, null);
				} catch (CoreException e) {
					Logger.log(LogLevel.INFO, "Could not refresh resource");
				}
			}
		}*/
		return null;
	}

	/**
	 * must be overwritten by subclass.
	 * @return the selected file in editor or navigator
	 */
	abstract IFile getSelectedFile();
	
	/**
	 * finds the selection file in navigator.
	 * 
	 * @return the model file of the selected resource
	 */
	protected final IFile getModelFile() {

		IFile selectedFile = getSelectedFile();
		if (selectedFile != null && selectedFile.getName().endsWith(".adf")) {
			Analysis analysis = AnalysisUtil.readAnalysis(selectedFile
					.getLocation().toOSString());
			return analysis.getIFile();
		}
		return null;
	}
}
