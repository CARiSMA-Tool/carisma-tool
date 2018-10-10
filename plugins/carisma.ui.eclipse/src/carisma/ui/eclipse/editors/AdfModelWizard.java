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

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/**
 * Wizard for the creation of adf files.
 */
public class AdfModelWizard extends Wizard implements INewWizard {
	
	public static final String EXTENSION_ID = "carisma.ui.eclipse.editors.AdfModelWizardID";
	
	/**
	 * Wizard page for new file.
	 */
	private AdfModelWizardNewFileCreationPage createnewfilePage = null;
	
	/**
	 * 
	 */
	private AdfModelWizardDetailsPage detailsPage = null;
	
	/**
	 * 
	 */
	private IStructuredSelection selection;
	
	/**
	 * 
	 */
	private IWorkbench workbench;
	
	/**
	 * constant for the context id. 
	 */	
	protected static final String ADFWIZARDCONTEXTID = "AdfWizard";
	
	@Override
	public final void init(final IWorkbench iWorkbench, final IStructuredSelection iStructuresSelection) {
		this.workbench = iWorkbench;
		this.selection = iStructuresSelection;
		setWindowTitle("New analysis file");
	}

	@Override
	public final void addPages() {
		this.createnewfilePage = new AdfModelWizardNewFileCreationPage("newFilePage", this.selection);
		this.createnewfilePage.setPageComplete(false);
		
		this.detailsPage = new AdfModelWizardDetailsPage("DetailsPage", this.createnewfilePage);
		this.detailsPage.setPageComplete(false);

		addPage(this.detailsPage);
		addPage(this.createnewfilePage);
		
	}

	@Override
	public final boolean performFinish() {
		try {
			this.createnewfilePage.setFileExtension("adf");
			this.createnewfilePage.createNewFile();
			
			// create name of Analyse Editor taking the filename whitout the extension 
			String filenameWithoutExt = this.createnewfilePage.getTargetFile().getName();
			int index = filenameWithoutExt.lastIndexOf('.');
			if (index > 0 && index <= filenameWithoutExt.length() - 2) {
				filenameWithoutExt = filenameWithoutExt.substring(0, index);
			}  
			
			Analysis analysis = new Analysis(filenameWithoutExt, this.detailsPage.getModelType(), this.detailsPage.getSourceFile());
			AnalysisUtil.storeAnalysis(analysis, this.createnewfilePage.getTargetFile().getLocation().toString());

			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			IProject[] projects = workspaceRoot.getProjects();
			for (IProject projectToRefresh : projects) {
				projectToRefresh.refreshLocal(IResource.DEPTH_ONE, null);
			}
			
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "Could not refresh resource");
		}

		// Open an editor on the new file.
		IWorkbenchWindow workbenchWindow = this.workbench.getActiveWorkbenchWindow();
		IWorkbenchPage page = workbenchWindow.getActivePage();
		try {
			page.openEditor(new FileEditorInput(this.createnewfilePage.getTargetFile()),
				 this.workbench.getEditorRegistry().getDefaultEditor(this.createnewfilePage.getTargetFile().getFullPath().toString()).getId());					 	 
		} catch (PartInitException exception) {
			MessageDialog.openError(workbenchWindow.getShell(), "Analysis Editor", exception.getMessage());
			return false;
		}
		return true;
	}
	
	
}