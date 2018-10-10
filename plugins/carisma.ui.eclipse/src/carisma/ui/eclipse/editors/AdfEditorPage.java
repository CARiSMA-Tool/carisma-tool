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

import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.editor.FormPage;
import org.eclipse.ui.forms.widgets.ScrolledForm;

/**
 * One page of the adf editor.
 */
public class AdfEditorPage extends FormPage {
	
	/**
	 * Adf-MasterDetailsBlock.
	 */
	private AdfEditorMasterDetailsBlock block;
	
	/**
	 * Constructor.
	 * 
	 * @param editor the Analysis Editor
	 * @param controller instance of the AdfEditorController
	 */
	public AdfEditorPage(final AdfEditor editor, final AdfEditorController controller) {
		super(editor, "analysisEditor", "Analysis Editor");
		this.block = new AdfEditorMasterDetailsBlock(controller);
	}
		
	@Override
	protected final void createFormContent(final IManagedForm managedForm) {
		final ScrolledForm form = managedForm.getForm();
		form.setText("Analysis Editor");
		this.block.createContent(managedForm);
	}
}
