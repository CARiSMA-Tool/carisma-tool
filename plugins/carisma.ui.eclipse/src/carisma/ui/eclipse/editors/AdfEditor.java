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
package carisma.ui.eclipse.editors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.forms.editor.FormEditor;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/**
 * This class represents frontend to modify the analysis parameter and checks.
 */
public class AdfEditor extends FormEditor {
		
	public static final String EXTENSION_ID = "carisma.ui.eclipse.editors.AdfEditor";
	
	/**
	 * Corresponding analysis.
	 */
	private Analysis analysis;
	
	/**
	 * Corresponding instance of the AdfEditorController.
	 */
	private AdfEditorController controller;
	
	/**
	 * Dirty state of the editor.
	 */
	private boolean dirty;
		
	/**
	 * Constructor.
	 */
	public AdfEditor() {
		super();
		this.dirty = false;
		this.controller = new AdfEditorController(this, this.analysis);
	}
	
	/**
	 * Returns the corresponding analysis.
	 * 
	 * @return the analysis
	 */
	protected final AdfEditorController getController() {
		return this.controller;
	}
		
	/**
	 * Need to set the EditorInput explicit when pages are switched
	 * otherwise it is not working when more than one file(model) is opened and changed. 
	 */
	@Override
	protected final void initializePageSwitching() {
		super.initializePageSwitching();
		FileEditorInput editorInput = (FileEditorInput) this.getEditorInput();
		setInputWithNotify(editorInput);
	}

	/** 
	 * Adds current AdfEditor to new page.  
	 */	
	@Override
	protected final void addPages() {
		try {
			this.setPartName(getEditorInput().getName());
			addPage(new AdfEditorPage(this, this.controller));
		} catch (PartInitException e) {
			Logger.log(LogLevel.ERROR, "could not add page!", e);
		}
	}
	
	/** 
	 * Saves current page.
	 * 
	 * @param arg0 the corresponding IProgressMonitor
	 */
	@Override
	public final void doSave(final IProgressMonitor arg0) {
		saveAnalysis();
		commitPages(isDirty());
	}


	/** 
	 * Saves current page with another name and/or at another location.  
	 */
	@Override
	public final void doSaveAs() {
		SaveAsDialog sad = new SaveAsDialog(getSite().getShell());
		sad.open();
		sad.setHelpAvailable(true);
		IPath path = sad.getResult();
		if (path != null) {
			IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
			if (file != null) {
				IEditorInput editorInput = new FileEditorInput(file);
				setInputWithNotify(editorInput);
				setPartName(editorInput.getName());
				AnalysisUtil.storeAnalysis(this.analysis, file.getLocation().toOSString());
				saveAnalysis();
				commitPages(isDirty());
			}
		}
	}
	
	/** 
	 * Returns the dirty-state of this AdfEditor.
	 * 
	 * @return the dirty-state
	 */
	@Override
	public final boolean isSaveAsAllowed() {
		return isDirty();
	}
	
	/**
	 * Loads the analysis for selected file. 
	 * 
	 * @return the loaded analysis instance
	 */
	protected final Analysis loadAnalysis() {
		if (getEditorInput().getAdapter(IFile.class) != null) {
			IFile file = getEditorInput().getAdapter(IFile.class);
			this.analysis = AnalysisUtil.readAnalysis(file.getLocation().toOSString());
			return this.analysis;
		}
		return null;
	}
	
	/**
	 * Saves the analysis to file.
	 */
	protected final void saveAnalysis() {
		IFile file = getEditorInput().getAdapter(IFile.class);
		AnalysisUtil.storeAnalysis(this.analysis, file.getLocation().toOSString());
		// refresh resources
		try {
			file.refreshLocal(IResource.DEPTH_ZERO, null);
			setDirty(false);
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "could not refresh resource", e);
		}
	}
	
	protected final void saveAutomaticAnalysis(Analysis ana){
	
		IFile file = ana.getIFile();
		AnalysisUtil.storeAnalysis(this.analysis, file.getLocation().toOSString());
		// refresh resources
		try {
			file.refreshLocal(IResource.DEPTH_ZERO, null);
			setDirty(false);
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "could not refresh resource", e);
		}
	}
	
	/** 
	 * Sets editor's dirty-state.
	 * 
	 * @param newDirty the new dirty-state of the editor
	 **/
	protected final void setDirty(final boolean newDirty) {
		if (this.dirty != newDirty) {
			this.dirty = newDirty;
			super.editorDirtyStateChanged();
		}
	}
	
	/** 
	 * Returns the dirty-state of this AdfEditor.
	 * 
	 * @return the dirty-state
	 */
	@Override
	public final boolean isDirty() {
		return this.dirty;
	}	
}
