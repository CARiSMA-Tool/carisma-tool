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

import java.io.IOException;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.content.IContentDescription;
import org.eclipse.core.runtime.content.IContentType;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.PlatformUI;

import carisma.core.analysis.Analysis;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;
import carisma.ui.eclipse.preferences.Constants;
import carisma.ui.eclipse.preferences.pages.EditorPriorityList;


/**
 * Class reads from the preferences the chosen editor 
 * and brings it to the user. 
 */
public class EditorTranslator {
	
	/**
	 * The analysis for this translator.
	 */
	private Analysis analysis;
	
	//########################################################################################
	/**
	 * Constructor.
	 * @param analysis the analysis for this translator
	 */
	public EditorTranslator(final Analysis analysis) {
		this.analysis = analysis;
	}

	/**
	 * Opens the editor, which is selected in the adf editor dropdown menu, or the
	 * editors from the editor priority list.
	 * @param defaultEditor Is true, when the Eclipse default editor for this file should be opened
	 * @return An indicator for the success of the method
	 */
	public final boolean openEditor(final boolean defaultEditor) {
		EditorRegistry editorRegistry = CarismaGUI.INSTANCE.getEditorRegistry();
		String editorSelectionArt = 
				CarismaGUI.INSTANCE.getPreferenceStore().getString(Constants.EDITOR_SELECTION_ART);
		// Priority list
		if (editorSelectionArt.equals(Constants.AUTO)) {
			List<String> editorPriorityList = EditorPriorityList.getPriorityList(this.analysis.getModelType());
			if (editorPriorityList == null || editorPriorityList.size() < 1) {
				MessageDialog mDialog = new MessageDialog(Display.getDefault().getActiveShell(),
						"Error", null, 
						"The editor priority list is empty\n"
						+ "Add editors to the list in the CARiSMA Preferences", 
						MessageDialog.ERROR, 
						new String[]{"OK"}, 0);
				mDialog.open();
				return false;
			}
			for (String editorName : editorPriorityList) {
				EditorDescriptor editorDesc = editorRegistry.getEditorDescriptorByName(editorName);
				if (editorDesc.forceOpen(this.analysis.getIFile())) {
					return true;
				}
			}
		// Dropdown menu selection
		} else {
			EditorDescriptor editorDesc = null;
			if (defaultEditor) {
				// Find Eclipse default editor for this model type
				editorDesc = getDefaultEditor();
			} else {
				// Find selected editor in the editor registry
				String selectedEditorId = this.analysis.getSelectedEditorId();
				editorDesc = editorRegistry.getEditorDescriptorById(selectedEditorId);
			}
			if (editorDesc != null) {
				if (editorDesc.forceOpen(this.analysis.getIFile())) {
					return true;
				}
				MessageDialog mDialog = new MessageDialog(Display.getDefault().getActiveShell(),
						"Error", null, 
						"The selected editor cannot be opened", 
						MessageDialog.ERROR, 
						new String[]{"OK"}, 0);
				mDialog.open();
				return false;
			}
			MessageDialog mDialog = new MessageDialog(Display.getDefault().getActiveShell(),
					"Error", null, 
					"The selected editor was not found", 
					MessageDialog.ERROR, 
					new String[]{"OK"}, 0);
			mDialog.open();
			return false;
		}
		return false;
	}
	
	/**
	 * Gets the Eclipse default editor for the selected file and
	 * returns the corresponding {@EditorDescriptor}. 
	 * @return EditorDescriptor The corresponding {@EditorDescriptor}
	 */
	public final EditorDescriptor getDefaultEditor() {
		IContentDescription contentDescription = null;
		try {
			IFile currentFile = this.analysis.getIFile();
			//Workaround for *.bpmn suffix
			if (currentFile.getFileExtension().equals("bpmn")) {
				contentDescription = Platform.getContentTypeManager()
						.getDescriptionFor(currentFile.getContents(), currentFile.getName() + "2", IContentDescription.ALL);
			} else {
				contentDescription = currentFile.getContentDescription();
			}
		} catch (CoreException e) {
		    Logger.log(LogLevel.ERROR, e.getMessage(), e);
		} catch (IOException e) {
			Logger.log(LogLevel.ERROR, e.getMessage(), e);
		}
		if (contentDescription != null) {
			IEditorDescriptor desc = null;
			IContentType contentType = contentDescription.getContentType();
			if (contentType != null) {
				desc = PlatformUI.getWorkbench().getEditorRegistry().getDefaultEditor(this.analysis.getIFile().getName(), contentType);
				return CarismaGUI.INSTANCE.getEditorRegistry().getEditorDescriptorById(desc.getId());
			}
		}
		return null;
	}
}
