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
package carisma.modeltype.bpmn2.popup;

import java.io.File;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.core.util.Utils;
import carisma.modeltype.bpmn2.yaoqiang.YaoqiangHelper;

/**
 * Context Action for converting an EMF to a Yaoqiang Model.
 * @author Marcel Michel
 *
 */
public class ConvertEmfAction implements IObjectActionDelegate {

	/**
	 * The shell.
	 */
	private Shell shell;
	/**
	 * The selected File. 
	 */
	private IFile selectedFile;
	
	/**
	 * The suffix, which should be added during the transformation.
	 */
	public static final String SUFFIX = "_yaoqiang";
	
	/**
	 * Constructor for ConvertYaoqiangAction. 
	 */
	public ConvertEmfAction() {
		super();
	}
	
	/**
	 * @see IActionDelegate#run(IAction)
	 */
	@Override
	public final void run(final IAction action) {
		if (this.selectedFile != null) {
			String ext = "." + this.selectedFile.getFileExtension();	
			String name = this.selectedFile.getName().substring(0, this.selectedFile.getName().indexOf(ext));
			String updatedName = name;
			String path = this.selectedFile.getLocation().toString().substring(0, this.selectedFile.getLocation().toString().indexOf(name));
			String outputfile = "";
			
			if (name.length() > ConvertYaoqiangAction.SUFFIX.length() 
					&& name.substring(name.length() - ConvertYaoqiangAction.SUFFIX.length(), name.length()).equals(ConvertYaoqiangAction.SUFFIX)) {
				updatedName = name.substring(0, name.length() - ConvertYaoqiangAction.SUFFIX.length());
			}
			if (!updatedName.contains(SUFFIX)) {
				outputfile = Utils.incrementFileNameIfNecessary(path + updatedName + SUFFIX + ".bpmn");
			} else {
				outputfile = Utils.incrementFileNameIfNecessary(path + updatedName + ".bpmn");
			}
			startEmfTransformation(path + name + ext, outputfile);
		} else {
			MessageDialog.openError(this.shell, "Carisma", "No file selected!");
		}
		// refresh resources
		try {
			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			IProject[] projects = workspaceRoot.getProjects();
			for (IProject projectToRefresh : projects) {
				projectToRefresh.refreshLocal(IResource.DEPTH_INFINITE, null);
			}
		} catch (CoreException e) {
			Logger.log(LogLevel.ERROR, "could not refresh resource");
		}
	}

	/**
	 * 
	 * @param inputFile
	 * @param outputFile
	 */
	private void startEmfTransformation(String inputFile, String outputFile) {
		File model = new File(inputFile);
		if (YaoqiangHelper.emf2yaoqiangModel(inputFile, outputFile)) {
			MessageDialog.openInformation(
					this.shell,
					"CARiSMA",
					"Successfully transformed model " + model.getName() + ".");
		} else {
			MessageDialog.openError(this.shell, "Carisma", "Error during transformation!");
		}
	}

	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 */
	@Override
	public void selectionChanged(final IAction action, final ISelection selection) {
		if (selection instanceof IStructuredSelection) {
            IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            this.selectedFile = (IFile) structuredSelection.getFirstElement();
        }
	}

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 */
	@Override
	public void setActivePart(final IAction action, final IWorkbenchPart targetPart) {
		this.shell = targetPart.getSite().getShell();
	}

}
