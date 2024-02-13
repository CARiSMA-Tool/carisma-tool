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

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.preferences.Constants;


/**
 * Class for preference page of CARiSMA.
 */
public class Carisma_index extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

    public static final String EXTENSION_ID = "carisma.ui.eclipse.index";
	
    /**
     * Constant String for Page description.
     */
    private static final String PAGE_DESCRIPTION = "Welcome to the main preference page.";
	/**
	 * editors.
	 */
	EditorRadioGroupFieldEditor editors;
	/**
	 * Composite top.
	 */
	private Composite top = null;
	/**
	 * Group for add/remove/up/down-Buttons and list.
	 */
	private Group edGroup;
	/**
	 * FieldEditor for add/remove/up/down-Buttons and list.
	 */
	private EditorPriorityList list;
	
	/**
	 * Consturctor.
	 */
	public Carisma_index() {
		super(GRID);
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
		setDescription(PAGE_DESCRIPTION);
	}

	/**
	 * Constructor.
	 * @param style style
	 */
	public Carisma_index(final int style) {
		super(style);
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
		setDescription(PAGE_DESCRIPTION);
	}

	/**
	 * Constructor.
	 * @param title title
	 * @param style style
	 */
	public Carisma_index(final String title, final int style) {
		super(title, style);
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
		setDescription(PAGE_DESCRIPTION);
	}

	/**
	 * Constructor.
	 * @param title title
	 * @param image image
	 * @param style style
	 */
	public Carisma_index(final String title, final ImageDescriptor image, final int style) {
		super(title, image, style);
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
		setDescription(PAGE_DESCRIPTION);
	}

	/**
	 * Initialize.
	 * @param workbench workbench
	 */
	@Override
	public final void init(final IWorkbench workbench) {
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
	}

	/**
	 * 
	 */
	@Override
	protected void createFieldEditors() {
		// addField(new BooleanFieldEditor(Constants.PREF_ANALYSE,
		// "Run every open analysis when starting CARiSMA?.",
		// getFieldEditorParent()));
		
	}

	/**
	 * @param parent parent
	 * @return Control
	 */
	@Override
	protected final Control createContents(final Composite parent) {
		this.top = new Composite(parent, SWT.LEFT);

		this.top.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.top.setLayout(new GridLayout());

		selectEditor(this.top);

		return this.top;
	}

	/**
	 * 
	 * @param composite the composite
	 */
	private void selectEditor(final Composite composite) {
		
		this.editors = new EditorRadioGroupFieldEditor(Constants.EDITOR_SELECTION_ART, 
				"Open a Model with", 1, new String[][] {
				{"&Editor selection combo box", Constants.MANUALLY},
				{"&Use editor priority list", Constants.AUTO},
		}, composite, true, getPreferenceStore().getString(Constants.EDITOR_SELECTION_ART));
				
		this.editors.setPage(this);
		this.editors.setPreferenceStore(getPreferenceStore());
		this.editors.load();
		
		this.edGroup = new Group(composite, SWT.NONE);
		
		this.list = new EditorPriorityList(
				Constants.EDITORS_LIST,
				"Editor priority list",
				this.edGroup); 
		this.list.setPage(this);
		this.list.setPreferenceStore(getPreferenceStore());
		this.list.load();
		
		selectionArtChanged(getPreferenceStore().getString(Constants.EDITOR_SELECTION_ART));
		
		this.editors.setPropertyChangeListener(new IPropertyChangeListener() {	
			@Override
			public void propertyChange(final PropertyChangeEvent event) {
				if (!event.getNewValue().toString().equals(event.getOldValue().toString())) {					
//					updateApplyButton();					
					String newSelectionIdValue = event.getNewValue().toString();
					Carisma_index.this.editors.setEditorSelectionId(newSelectionIdValue);
					selectionArtChanged(newSelectionIdValue);
				}
			}
		});		
	} 

	/**
	 * Save the state.
	 * @return boolean
	 */
	@Override
	public final boolean performOk() {
		if (this.editors != null) {
			this.editors.store();
		}
		if (this.list != null) {
			this.list.store();
		}
		return super.performOk();
	}
	
	@Override
	public final void performApply() {
		super.performApply();
	}

	/**
	 * @return boolean 
	 */
	@Override
	public final boolean isValid() {
	
		if (this.editors != null) {			
			if (!this.editors.isValid()) {
				setErrorMessage("Editor is not active or installed");
				return false;
			}
			setErrorMessage(null);
		}
		return super.isValid();
		
	}

	/**
	 * loads default values of all FieldEditors.
	 */
	@Override
	protected final void performDefaults() {
		this.editors.loadDefault();
		this.list.loadDefault();		
		selectionArtChanged(getPreferenceStore().getString(Constants.EDITOR_ID));
		super.performDefaults();
	}

	/**
	 * 
	 * @return top
	 */
	public final Composite getTop() {
		return this.top;
	}

	/**
	 * @param newValue the new value of Selection art
	 */
	void selectionArtChanged(final String newValue) {
		
		if (newValue.equals(Constants.AUTO)) {
			if (this.list != null) {
				//TODO init
			}
			this.edGroup.setEnabled(true);
			this.list.setEnabled(true, this.edGroup);
		} else {
			this.edGroup.setEnabled(false);
			this.list.setEnabled(false, this.edGroup);
		}
	}
}

