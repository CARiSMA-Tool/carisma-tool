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
import org.eclipse.core.commands.HandlerEvent;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.editors.AdfEditor;


/**
 * Abstract class for ForgetHandlers.
 */
public abstract class ForgetEditorHandler extends AbstractHandler {
	/**
	 * the selected resource.
	 */
	private IFile selectedFile;
	/**
	 * 
	 */
	private boolean alreadyInstalledSelectionListener = false;
	/**
	 * the Analysis.
	 */
	private Analysis analysis;

	
	/**
	 * @return the selected resource in navigator or editor 
	 */
	abstract IFile getSelectedFile();
	/**
	 * @return the subclass handler.
	 */
	abstract ForgetEditorHandler getHandler();
	/**
	 * initialize the listeners.
	 */
	abstract void installListener();
	
	/** Getter for alreadyInstalledSelectionListener.
	 * 
	 * @return the boolean value of alreadyInstalledSelectionListener.
	 */
	public final boolean getAlreadyInstalledSelectionListener() {
	    return this.alreadyInstalledSelectionListener;
	}
	
	/** Setter for alreadyInstalledSelectionListener.
	 * 
	 * @param newValue the new value for alreadyInstalledSelectionListener.
	 */
	public final void setAlreadyInstalledSelectionListener(final boolean newValue) {
	    this.alreadyInstalledSelectionListener = newValue;
	}

	
	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.IHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {

		this.analysis = AnalysisUtil.readAnalysis(this.selectedFile.getLocation()
				.toOSString());

		if (this.analysis == null) {
			return null;
		} else if (this.analysis.getSelectedEditorId() == null
				|| this.analysis.getSelectedEditorId().equals("")) {

			Display display = Display.getDefault();
			Shell shell = new Shell(display);
			MessageDialog mDialog = new MessageDialog(shell, "Editor not set",
					null, "No editor set for this analysis",
					MessageDialog.ERROR, new String[] { "OK" }, 0);
			mDialog.open();
			return null;
		}
		this.analysis.setSelectedEditorId(null);
		fireHandlerChanged(new HandlerEvent(getHandler(), true, false));
		saveChanges();
		
		IWorkbenchPage[] pages = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages();

		for (IWorkbenchPage page : pages) {
			for (IEditorReference editorRef : page.getEditorReferences()) {
				if (editorRef != null 
						&& this.selectedFile.getName().equals(editorRef.getName())
						&& editorRef.getEditor(false) instanceof AdfEditor) {
					IEditorInput editInput = ((AdfEditor) editorRef.getEditor(false)).getEditorInput();
					if (editInput.getAdapter(IFile.class) != null) {
						IFile file = editInput.getAdapter(IFile.class);
						this.analysis = AnalysisUtil.readAnalysis(file.getLocation().toOSString());
					}
				}
			}
		}

		return null;
	
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.commands.AbstractHandler#isEnabled()
	 */
	/**
	 * @return enabled, if the analysis has an saved editor name
	 */
	@Override
	public final boolean isEnabled() {
		this.selectedFile = getSelectedFile();
		if (this.selectedFile != null) {
			Analysis analysis = AnalysisUtil.readAnalysis(this.selectedFile
					.getLocation().toOSString());
			if (analysis.getSelectedEditorId() == null
					|| "".equals(analysis.getSelectedEditorId())) {
				setBaseEnabled(false);
				return false;
			}
			setBaseEnabled(true);
			return true;
		}
		return false;
	}


	@Override
	public final void setEnabled(final Object evaluationContext) {
		installListener();
	}


	
	/**
	 * save changes to resource.
	 */
	private void saveChanges() {
	    if (this.selectedFile == null) {
	        // TODO
	        return;
	    }
		Display display = Display.getDefault();
		Shell shell = new Shell(display);
		MessageDialog mDialog = new MessageDialog(shell, "Save and Launch",
				null, "Do you want to save the changes in"
						+ this.selectedFile.getLocation().toOSString() + "?", 0,
				new String[] { "OK", "Cancel" }, 0);
		mDialog.open();

		if (mDialog.getReturnCode() == 0) { // save analysis
			AnalysisUtil.storeAnalysis(this.analysis, this.selectedFile.getLocation()
					.toOSString());
			// refresh resource
			
			try {
				this.selectedFile.refreshLocal(IResource.DEPTH_ZERO, null);
			} catch (CoreException e) {
				Logger.log(LogLevel.INFO, "Could not refresh resource");
			
			}
		}
	}
}
