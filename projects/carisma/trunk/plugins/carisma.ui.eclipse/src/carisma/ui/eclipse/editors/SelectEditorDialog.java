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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListDialog;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;


/**
 * Dialog that is initialized with the available Editors for opening a model.
 */
public class SelectEditorDialog extends ListDialog {
	
	/**
	 * Shell.
	 */
	private Shell shell = null;
	
	/**
	 * Flag to remember a decision/selection. 
	 */
	boolean remember = false;
	
	/**
	 * A check button to remember.
	 */
	Button rememberButton;

	//########################################################################################
	/**
	 * Constructor.
	 * @param parent the Shell
	 * @param saveSelectionId the Id, that was saved in the analysis
	 * @param fileName the IFile is used to select Editors which are capable of opening it. 
	 */
	public SelectEditorDialog(final Shell parent, final String saveSelectionId, final IFile fileName) {
		super(parent);
		this.shell = parent;
		if (saveSelectionId != null && !"".equals(saveSelectionId)) {
			this.remember = true;
		}
		init(fileName);
	}

	/**
	 * Initialize Dialog.
	 * @param modelFile used to verify that the editor is capable of handling the file.
	 */
	public final void init(final IFile modelFile) {
		setTitle("Open the model with");
		setMessage("Select an editor");
		
		setContentProvider(ArrayContentProvider.getInstance());
		setLabelProvider(new LabelProvider());
		PlatformUI.getWorkbench().getHelpSystem().setHelp(this.shell, CarismaGUI.PLUGIN_ID + ".AdfEditor");
		setHelpAvailable(true); 
		
		List<String> input = new ArrayList<>();

		// insert editors from registry
		List<EditorDescriptor> eds = CarismaGUI.INSTANCE.getEditorRegistry().getRegisteredEditors();
		for (EditorDescriptor ed : eds) {
			if (ed.isAvailable() && ed.isApplicable(modelFile)) {
				input.add(ed.getName());
			}
		}
		setInput(input);
		
	}

	/**
	 * Creates the necessary buttons.
	 * @param parent Composite where the buttons are created
	 */
	@Override
	protected final void createButtonsForButtonBar(final Composite parent) {
		super.createButtonsForButtonBar(parent);
		
		this.rememberButton = new Button(parent, SWT.CHECK);
		this.rememberButton.setText("Remember my decision");
		this.rememberButton.setSelection(this.remember);
		this.rememberButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				SelectEditorDialog.this.remember = SelectEditorDialog.this.rememberButton.getSelection();
			}
		}); 
	}

	/**
	 * Getter for the remember flag.
	 * @return remember decision flag
	 */
	public final boolean getRemember() {	
		return this.remember;
	}

}
