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
package carisma.ui.eclipse.popup.actions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.analysis.CheckReference;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.CarismaGUI;


/**
 * An action to run analysis.
 */
public class RunAnalysisAction implements IObjectActionDelegate {

	/**
	 * The analysis.
	 */
	private Analysis analysis;
	
	/**
	 * The selected file.
	 */
	private IFile selectedFile;
	
	//########################################################################################
	@Override
	public final void run(final IAction action) {
		this.analysis = AnalysisUtil.readAnalysis(this.selectedFile.getLocation()
				.toOSString());
		List<CheckReference> checks = this.analysis.getChecks();

		if (!checks.isEmpty()) {
			List<CheckReference> unsetRequiredParameters = this.analysis.getChecksWithInvalidParameters();
			if (unsetRequiredParameters.size() > 0) {
				showUnsetRequiredParameters(unsetRequiredParameters);
			} else {
				CarismaGUI.runAnalysis(this.analysis);
			}
		} else {
			Display display = Display.getDefault();
			Shell shell = new Shell(display);
			TitleAreaDialog tad = new TitleAreaDialog(shell) {
				@Override
				protected Control createDialogArea(final Composite parent) {
					setTitle("ERROR: Check list is empty!");
					PlatformUI.getWorkbench().getHelpSystem().setHelp(parent, CarismaGUI.PLUGIN_ID + ".AdfEditor");
					setHelpAvailable(true);
					
					final Composite composite = (Composite) super.createDialogArea(parent);
					GridLayout compositeLayout = new GridLayout(1, false);
					compositeLayout.marginHeight = 2;
					compositeLayout.marginWidth = 10;
					composite.setLayout(compositeLayout);
					
					final Label label = new Label(composite, SWT.NONE);
					label.setText("There is no check in the list of the analysis.\n" +
							"Maybe the analysis is still open and there are unsaved changes.\n \n");
					final Link link = new Link(composite, SWT.NONE);
					link.setText("Read CARiSMA <A>Help</A> for more information.");					
					link.addSelectionListener(new SelectionAdapter() {
						@Override
						public void widgetSelected(final SelectionEvent e) {
//							PlatformUI.getWorkbench().getHelpSystem().displayHelpResource("/" + Carisma.PLUGIN_ID + "/help/html/maintopic.html");
							PlatformUI.getWorkbench().getHelpSystem().displayHelp();	
//							PlatformUI.getWorkbench().getHelpSystem().displayHelp(Carisma.PLUGIN_ID + ".AdfEditor");
							close();
						}
					});
					return composite;
				}
			};
			tad.create();			
			tad.open();			
		}

	}

	/**
	 * Checks for all checks together, if there are required parameter to be
	 * set. 
	 * @return list<CheckReference>
	 */
	private List<CheckReference> getChecksWithRequiredParameters() {

		List<CheckReference> checks = this.analysis.getChecks();
		List<CheckReference> checksWithRequiredParameters = new ArrayList<>();

		for (CheckReference check : checks) {
			if (!check.getUnsetRequiredParameters().isEmpty()) {
				checksWithRequiredParameters.add(check);
			}
		}

		return checksWithRequiredParameters;
	}

	/**
	 * Show a message with name of checks, where the required parameters are not
	 * filled, used to stop Run process if necessary.
	 * @param checksWithUnsetRequiredParameters A list of checks, for those required parameter are not filled
	 */
	private static void showUnsetRequiredParameters(
			final List<CheckReference> checksWithUnsetRequiredParameters) {
		StringBuffer message = new StringBuffer("Required parameters not set in: \n");

		if (checksWithUnsetRequiredParameters.size() > 0) {
			for (CheckReference check : checksWithUnsetRequiredParameters) {
				message.append("\"");
				message.append(check.getCheckID());
				message.append("\"\n");
			}
			Display display = Display.getDefault();
			Shell shell = new Shell(display);
			MessageDialog mDialog = new MessageDialog(shell,
					"Unset required parameter error", null, message.toString(),
					MessageDialog.ERROR, new String[] { "OK" }, 0);
			mDialog.open();
		}
	}

	@Override
	public final void selectionChanged(final IAction action, final ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			this.selectedFile = (IFile) structuredSelection.getFirstElement();
		}

	}

	@Override
	public final void setActivePart(final IAction action, final IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * Initialization by Handler.
	 * @return true, if successful (e.g. an editor is opened and the opened resource has an "adf" extension)
	 */
	public final boolean initSelectionByEditor() {
		IWorkbenchPage page = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	    try {
	    	FileEditorInput editorInput = (FileEditorInput) page.getActiveEditor().getEditorInput();
	    	this.selectedFile = editorInput.getFile();
	    	
	    	if (this.selectedFile.getName().endsWith(".adf")) {
	    		return true;
	    	}
	    } catch (NullPointerException npe) {
	    	// nothing selected, do nothing
	    	Logger.log(LogLevel.INFO, "No resource selected");
	    }
	    return false;
	}

	/**
	 * Selection in Project navigator, not in editor. 
	 * @return true, if an resource was selected
	 */
	public final boolean initSelectionByService() {
		try {
			IWorkbenchWindow ww = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			ISelection selection = ww.getSelectionService().getSelection();
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			this.selectedFile = (IFile) structuredSelection.getFirstElement();
			
			return true;
			
		} catch (NullPointerException npe) {
			Logger.log(LogLevel.INFO, "No resource selected");
		}
		return false;
	}
}
