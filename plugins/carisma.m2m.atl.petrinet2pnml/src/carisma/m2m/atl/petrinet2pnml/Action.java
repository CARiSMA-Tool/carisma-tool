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
package carisma.m2m.atl.petrinet2pnml;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.m2m.atl.common.ATLExecutionException;
import org.eclipse.m2m.atl.core.ATLCoreException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

import carisma.core.util.Utils;
import carisma.m2m.atl.petrinet2pnml.files.PNML2XML;
import carisma.m2m.atl.petrinet2pnml.files.Petrinet2PNML;
import carisma.m2m.atl.petrinet2pnml.files.XML2Text;

/**
 * Transformation Template.
 * 
 * @author Marcel Michel
 */
public class Action implements IObjectActionDelegate {

	/**
	 * currentSelection.
	 */
	private ISelection currentSelection;

	/**
	 * Constructor for Action.
	 */
	public Action() {
		super();
	}

	/**
	 * The corresponding shell.
	 */
	private Shell shell;
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
	 *      org.eclipse.ui.IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = targetPart.getSite().getShell();
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		IStructuredSelection iss = (IStructuredSelection) currentSelection;
		for (Iterator<?> iterator = iss.iterator(); iterator.hasNext();) {
			try {
				transform((IFile)iterator.next());
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}
	}

	private void transform(IFile file) throws Exception {
		String filepath = file.getLocation().toString().substring(0, file.getLocation().toString().indexOf(file.getFileExtension()) - 1);
		
		boolean errorOccurred = false;
		try {
			String pnmlFilepath = Utils.incrementFileNameIfNecessary(filepath + ".pnml.tmp");
			File pnmlFile = new File(pnmlFilepath);
			Petrinet2PNML.main(new String[]{"file:///" + file.getLocation().toFile().toString(), 
					"file:///" + pnmlFile.getAbsoluteFile().toString()});
			
			String xmlFilepath = Utils.incrementFileNameIfNecessary(filepath + ".xml.tmp");
			File xmlFile = new File(xmlFilepath);
			PNML2XML.main(new String[] {"file:///" + pnmlFile.getAbsoluteFile().toString(), 
					"file:///" + xmlFile.getAbsoluteFile().toString()});
			
			XML2Text.main(new String[]{"file:///" + xmlFile.getAbsoluteFile().toString()});
			
			// Delete temporary files
			pnmlFile.delete();
			xmlFile.delete();

		} catch (ATLCoreException e) {
			e.printStackTrace();
			errorOccurred = true;
		} catch (IOException e) {
			e.printStackTrace();
			errorOccurred = true;
		} catch (ATLExecutionException e) {
			e.printStackTrace();
			errorOccurred = true;
		} finally {
			if (errorOccurred) {
				MessageDialog.openError(
						shell,
						"CARiSMA",
						"Transformation failed!");
			} else {
				MessageDialog.openInformation(
						shell,
						"CARiSMA",
						"Transformation successfully performed!");
			}
		}
	}

	/**
	 * {@inheritDoc}
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		this.currentSelection = selection;
	}
}
