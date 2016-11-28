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
package carisma.ui.eclipse.preferences.pages;

import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.swt.widgets.Composite;

import carisma.ui.eclipse.preferences.Constants;


/**
 * Class for EditorRadioGroup Field Editor used in Carisma preferences.
 */
public class EditorRadioGroupFieldEditor extends RadioGroupFieldEditor {

	/**
	 * the id of the selected editor.
	 */
	private String editorId = "";
	/**
	 * 
	 */
	private String editorSelectionId = Constants.MANUALLY;
	/**
	 * @see RadioGroupFieldEditor
	 * @param name preference name
	 * @param labelText label text
	 * @param numColumns number of columns
	 * @param labelAndValues label and values
	 * @param parent parent Composite
	 * @param useGroup whether to use a Group control to contain the radio buttons
	 * @param editorId 
	 */
	public EditorRadioGroupFieldEditor(final String name, final String labelText,
			final int numColumns, final String[][] labelAndValues, final Composite parent,
			final boolean useGroup, final String editorId) {

		super(name, labelText, numColumns, labelAndValues, parent,
				useGroup);
		this.editorId = editorId;
	}

	/**
	 * set an editor Id.
	 * @param id Id of the chosen editor
	 */
	public final void setEditorId(final String id) {
		this.editorId = id;
	}
	
	/**
	 * 
	 * @return Id of this editor
	 */
	public final String getEditorId() {
		return this.editorId;
	}
	/**
	 * Id of selection Art (e.g. "default", "manually")
	 * @param id 
	 */
	public final void setEditorSelectionId(final String id) {
		this.editorSelectionId = id;
	}
	/**
	 * 
	 * @return Id of selection Art (e.g. "default", "manually")
	 */
	public final String getEditorSelectionId() {
		return this.editorSelectionId;
	}
}
