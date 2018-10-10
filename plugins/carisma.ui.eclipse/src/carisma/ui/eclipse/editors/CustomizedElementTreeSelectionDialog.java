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

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ElementTreeSelectionDialog;

import carisma.ui.eclipse.CarismaGUI;


/**
 * ElementTreeSelectionDialog with customized help.
 */
public class CustomizedElementTreeSelectionDialog extends
		ElementTreeSelectionDialog {
	
	/**
	 * Constructor.
	 * @param parent the parent shell
	 * @param labelProvider the ILabelProvider
	 * @param contentProvider the ITreeContentProvider
	 */
	public CustomizedElementTreeSelectionDialog(final Shell parent,
        final ILabelProvider labelProvider, final ITreeContentProvider contentProvider) {
		super(parent, labelProvider, contentProvider);
	}
	
	@Override
	protected final void configureShell(final Shell shell) {
		super.configureShell(shell);		
		PlatformUI.getWorkbench().getHelpSystem().setHelp(shell, CarismaGUI.PLUGIN_ID + ".AdfEditor"); 
	}
	
}
