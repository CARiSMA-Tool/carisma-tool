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
package carisma.ui.eclipse.dialogs.parameters;

import java.io.File;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;

import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.checks.CheckDescriptor;


/**
 * Dialog to set the parameters.
 */
public class ParameterDialogUtil extends Dialog {
	
	/**
	 * Constant text for all parameter.
	 */
	private static final String DIALOG_TEXT = "Set value for ";
	
	/**
	 * The CheckDescriptor.
	 */
	private CheckDescriptor checkDescriptor = null;
	
	//########################################################################################
	/**
	 * Constructor.
	 * @param parentShell the shell
	 * @param cd the CheckDescriptor, whose parameter must be asked
	 */
	public ParameterDialogUtil(final Shell parentShell, final CheckDescriptor cd) {
		super(parentShell);
		this.checkDescriptor = cd;
	}

	/**
	 * Getter for a String Parameter.
	 * @param parameter String parameter
	 * @return the new value of the String Parameter
	 */
	public final String queryStringParameter(final StringParameter parameter) {
		InputDialog id = new InputDialog(super.getShell(), this.checkDescriptor.getName(),
				DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (STRING)", 
				parameter.getValue(),
				null);
		int result = id.open();
		if (result == 0) {		// OK-Button
			return id.getValue();
		}
		// Cancel
		return parameter.getValue();
	}

	/**
	 * Getter for a Float Parameter.
	 * @param parameter Float Parameter
	 * @return the new value of the Float Parameter
	 */
	public final float queryFloatParameter(final FloatParameter parameter) {
		final InputDialog inputDialog;
		
		IInputValidator inputValidator = new IInputValidator() {
			@Override
			public String isValid(final String newText) {
				try {
					Float.parseFloat(newText);
					return null;
				} catch (Exception e) {
					return "Please insert a float value!";
				}
			}
		};
		
		inputDialog = new InputDialog(super.getShell(), this.checkDescriptor.getName(),
				DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (FLOAT)", 
				String.valueOf(parameter.getValue()),
				inputValidator);
		int result = inputDialog.open();
		if (result == 0) {		// OK-Button
			return Float.parseFloat(inputDialog.getValue());
		}
		// Cancel
		return parameter.getValue();
	}

	/**
	 * Getter for a Integer Parameter.
	 * @param parameter Integer Parameter
	 * @return the new value of the Integer Parameter
	 */
	public final int queryIntegerParameter(final IntegerParameter parameter) {
		final InputDialog id;
		IInputValidator iv = new IInputValidator() {
			@Override
			public String isValid(final String newText) {
				try {
					Integer.parseInt(newText);
					return null;
				} catch (Exception e) {
					return "Please insert an integer value!";
				}
			}
		};
		
		id = new InputDialog(super.getShell(), this.checkDescriptor.getName(),
				DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (INTEGER)",
				String.valueOf(parameter.getValue()), 
				iv);
		
		boolean isANumber = false;

		while (!isANumber) {
			isANumber = true;
			int result = id.open();
			if (result == 1) {		// Cancel
				return parameter.getValue();	
			}
			// OK-Button
			String value = id.getValue();
			Integer valueInt = null;
			try {
				valueInt = Integer.valueOf(value);
			} catch (NumberFormatException nfe) {
				isANumber = false;
			}
			if (isANumber) {
				return valueInt.intValue();
			}
		}
		return parameter.getValue();
	}

	/**
	 * Getter for a Boolean Parameter.
	 * @param parameter Boolean Parameter
	 * @return the new value of the Boolean Parameter
	 */
	public final boolean queryBooleanParameter(final BooleanParameter parameter) {
		int result = 0;
		if (!parameter.getValue()) { 	// defaultValue used for MessageDialog's
			result = 1;
		}
		MessageDialog md = new MessageDialog(super.getShell(),
				this.checkDescriptor.getName(), null,
				DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (BOOLEAN)", 
				0, 
				new String[] {"True", "False" }, 
				result);
		result = md.open();
		
		return result == 0;

	}

	/**
	 * Getter for a InputFile Parameter.
	 * @param parameter InputFile Parameter
	 * @return the new value of the InputFileParameter
	 */
	public final File queryInputFileParameter(final InputFileParameter parameter) {
		FileDialog fDialog = new FileDialog(super.getParentShell());
		if (parameter.getValue() != null) {
			fDialog.setFileName(parameter.getValue().toString());
		}
		fDialog.setText(this.checkDescriptor.getName() + ": " + DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (INPUTFILE)");
		String selected = fDialog.open();
		if (selected != null) {
			return new File(selected);
		}
		return parameter.getValue();
	}

	/**
	 * Getter for a Folder Parameter.
	 * @param parameter FolderParameter
	 * @return the new value of FolderParameter
	 */
	public final File queryFolderParameter(final FolderParameter parameter) {
		DirectoryDialog dirDialog = new DirectoryDialog(super.getParentShell());
		final String folderValue; 
		if (parameter.getValue() != null) {
			folderValue = parameter.getValue().getPath();
		} else {
			folderValue = "";
		}
		dirDialog.setText(this.checkDescriptor.getName() + ": " + DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (FOLDER)");
		dirDialog.setFilterPath(folderValue);
		String result = dirDialog.open();
		if (result != null && !"".equals(result)) {
			return new File(result);
		}
		return parameter.getValue();
	}
	
	/**
	 * Getter for a OutputFile Parameter.
	 * @param parameter OutputFile
	 * @return the new value of OutputFileParameter
	 */
	public final File queryOutputFileParameter(final OutputFileParameter parameter) {
		FileDialog fDialog = new FileDialog(super.getParentShell(), SWT.SAVE);
		fDialog.setOverwrite(true);
		fDialog.setText(this.checkDescriptor.getName() + ": " + DIALOG_TEXT + "\"" + parameter.getDescriptor().getName() + "\" (OUTPUTFILE)");
		String selection;
		
		if (parameter.getValue() != null) {
			selection = parameter.getValue().getPath();
			fDialog.setFileName(selection);
		} else {
			selection = "";
		}
		selection = fDialog.open();
		if (selection != null && !"".equals(selection)) {
			return new File(selection);
		}
		return parameter.getValue();
	}
}
