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
	public final void init(final IWorkbench workbench, final IStructuredSelection selection) {
		this.workbench = workbench;
		this.selection = selection;
		setWindowTitle("New analysis file");
	}

	@Override
	public final void addPages() {
		createnewfilePage = new AdfModelWizardNewFileCreationPage("newFilePage", selection);
		createnewfilePage.setPageComplete(false);
		
		detailsPage = new AdfModelWizardDetailsPage("DetailsPage", createnewfilePage);
		detailsPage.setPageComplete(false);

		addPage(detailsPage);
		addPage(createnewfilePage);
		
	}

	@Override
	public final boolean performFinish() {
		try {
			createnewfilePage.setFileExtension("adf");
			createnewfilePage.createNewFile();
			
			// create name of Analyse Editor taking the filename whitout the extension 
			String filenameWithoutExt = createnewfilePage.getTargetFile().getName();
			int index = filenameWithoutExt.lastIndexOf('.');
			if (index > 0 && index <= filenameWithoutExt.length() - 2) {
				filenameWithoutExt = filenameWithoutExt.substring(0, index);
			}  
			
			Analysis analysis = new Analysis(filenameWithoutExt, detailsPage.getModelType(), detailsPage.getSourceFile());
			AnalysisUtil.storeAnalysis(analysis, createnewfilePage.getTargetFile().getLocation().toString());

			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			IProject[] projects = workspaceRoot.getProjects();
			for (IProject projectToRefresh : projects) {
				projectToRefresh.refreshLocal(IProject.DEPTH_ONE, null);
			}
			
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "Could not refresh resource");
		}

		// Open an editor on the new file.
		IWorkbenchWindow workbenchWindow = workbench.getActiveWorkbenchWindow();
		IWorkbenchPage page = workbenchWindow.getActivePage();
		try {
			page.openEditor(new FileEditorInput(createnewfilePage.getTargetFile()),
				 workbench.getEditorRegistry().getDefaultEditor(createnewfilePage.getTargetFile().getFullPath().toString()).getId());					 	 
		} catch (PartInitException exception) {
			MessageDialog.openError(workbenchWindow.getShell(), "Analysis Editor", exception.getMessage());
			return false;
		}
		return true;
	}
	
	
}