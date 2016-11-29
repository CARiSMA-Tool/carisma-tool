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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.ui.dialogs.ListDialog;

import carisma.core.Carisma;
import carisma.core.models.ModelType;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;
import carisma.ui.eclipse.editors.descriptions.TextEditorDescriptor;
import carisma.ui.eclipse.preferences.Constants;


/**
 * A field editor for displaying and storing a list of strings. Buttons are
 * provided for adding items to the list and removing items from the list.
 */
public class EditorPriorityList extends FieldEditor {

	/**
	 * Button Add.
	 */
	private Button btAdd = null;
	/**
	 * Button Remove. (cannot be null-> NullPointerException)
	 */
	private Button btRemove = null;
	/**
	 * Button Down. (cannot be null-> NullPointerException)
	 */
	private Button btDown = null;
	/**
	 * Button Up. (cannot be null-> NullPointerException)
	 */
	private Button btUp = null;
	/**
	 * Model type label.
	 */
	private Label modelTypeLabel = null;
	/**
	 * Title label.
	 */
	private Label titleLabel = null;
	/**
	 * Note label1.
	 */
	private Label noteLabel1 = null;
	/**
	 * Note label2.
	 */
	private Label noteLabel2 = null;
	/**
	 * Composite container for the Field Editor.
	 */
	private Composite container = null;
	/**
	 * List Control for the items. 
	 */
	List guiList = null;
	/** 
	 * Constant to separate different entries.
	 */
	private static final String SEPARATOR = ";";
	/**
	 * Constant to separate List entries.
	 */
	private static final String SEPARATOR_LIST = ",";
	/**
	 * Constant to identify the model type.
	 */
	private static final String SELECTED_MODELTYPE = "MODELTYPE=";
	/**
	 * Constant to identify list items.
	 */
	private static final String LIST_ITEMS = "LIST_ITEMS=";
	/**
	 * Combo box for available model types.
	 */
	Combo modelTypeCombo = null;
	/**
	 * List of available model types.
	 */
	private java.util.List<ModelType> listModelTypes = null;
	/**
	 * Structure to manage model types and selected editors. 
	 */
	HashMap<String, ArrayList<String>> selectedEditors = null;
	
	/**
	 * Konstruktor.
	 * @param name the name of the FieldEditor in the Preference Page
	 * @param labelText Label
	 * @param parent parent
	 */
	public EditorPriorityList(final String name, final String labelText,
			final Composite parent) {
		init(name, labelText);
		createControl(parent);
		
		this.container = parent;
	}

	@Override
	protected final void adjustForNumColumns(final int numColumns) {

		((GridData) this.container.getLayoutData()).horizontalSpan = numColumns;
	}

	@Override
	protected final void doFillIntoGrid(final Composite parent, final int numColumns) {
		this.container = parent;
		GridLayout layout = new GridLayout(2, false);
		this.container.setLayout(layout);
		this.container.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		
		
		this.modelTypeLabel = new Label(this.container, SWT.LEFT);
		this.modelTypeLabel.setText("Select ModelType:");
		this.modelTypeCombo = new Combo(this.container, SWT.READ_ONLY);

		this.modelTypeCombo.addSelectionListener(new SelectionListener() {
			
			@Override
			public void widgetSelected(final SelectionEvent e) {
				updatePriorityList();
				selectionChanged();
			}
			
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				updatePriorityList();
				selectionChanged();
			}
		});

		this.titleLabel = getLabelControl(this.container);
		GridData gdTitleLabel = new GridData(GridData.FILL_HORIZONTAL);
		gdTitleLabel.horizontalSpan = 2;
		this.titleLabel.setLayoutData(gdTitleLabel);		

		this.guiList = new List(this.container, SWT.BORDER);
		GridData listGd = new GridData(SWT.FILL, SWT.FILL, true, true); 
		listGd.verticalAlignment = GridData.FILL;
		listGd.horizontalAlignment = GridData.FILL;
		this.guiList.setLayoutData(listGd);
		this.guiList.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				selectionChanged(); 				
			}
		});	

		Composite addRemoveGroup = new Composite(this.container, SWT.NONE);
		addRemoveGroup.setLayoutData(new GridData());

		GridLayout addRemoveLayout = new GridLayout();
		addRemoveLayout.numColumns = 2;
		addRemoveLayout.marginHeight = 0;
		addRemoveLayout.marginWidth = 0;
		addRemoveGroup.setLayout(addRemoveLayout);

		Composite buttonGroup = new Composite(addRemoveGroup, SWT.NONE);
		buttonGroup.setLayoutData(new GridData(SWT.RIGHT));
		GridLayout buttonLayout = new GridLayout();
		buttonLayout.marginHeight = 0;
		buttonLayout.marginWidth = 0;
		buttonGroup.setLayout(buttonLayout);

		this.btAdd = new Button(buttonGroup, SWT.PUSH);
		this.btAdd.setText("Add");
		this.btAdd.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.btAdd.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				super.widgetSelected(e);
				String result = openEditorSelectionDialog();
				if (result != null) {
					EditorPriorityList.this.guiList.add(result);
					ArrayList<String> list = EditorPriorityList.this.selectedEditors.get(EditorPriorityList.this.modelTypeCombo.getText());
					if (list != null) {
						list.add(result);
					} else {
						ArrayList<String> editor = new ArrayList<>();
						editor.add(result);
						EditorPriorityList.this.selectedEditors.put(EditorPriorityList.this.modelTypeCombo.getText(), editor);
					}
					selectionChanged();
				}
			}
		});			
			
		this.btRemove = new Button(buttonGroup, SWT.PUSH);
		this.btRemove.setText("Remove");
		this.btRemove.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		this.btRemove.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				super.widgetSelected(e);
				int selection = EditorPriorityList.this.guiList.getSelectionIndex();
				if (selection > -1) {
					java.util.List<String> editors = EditorPriorityList.this.selectedEditors.get(EditorPriorityList.this.modelTypeCombo.getText());
					editors.remove(EditorPriorityList.this.guiList.getItem(EditorPriorityList.this.guiList.getSelectionIndex()));
					
					EditorPriorityList.this.guiList.remove(EditorPriorityList.this.guiList.getSelectionIndex());
					selectionChanged();
				}
			}
		});
		
		this.btUp = new Button(buttonGroup, SWT.PUSH);
		this.btUp.setText("Up");
		this.btUp.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.btUp.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				super.widgetSelected(e);
				int index = EditorPriorityList.this.guiList.getSelectionIndex();
				if (index > 0) {					
					String selectedItem = EditorPriorityList.this.guiList.getItem(index); 				
					String itemAboveSelected = EditorPriorityList.this.guiList.getItem(index - 1); 
					
					java.util.List<String> editors = EditorPriorityList.this.selectedEditors.get(EditorPriorityList.this.modelTypeCombo.getText());
					editors.add(index - 1, editors.remove(index));
					
					EditorPriorityList.this.guiList.add(selectedItem, index - 1);					
					EditorPriorityList.this.guiList.remove(index);					
					EditorPriorityList.this.guiList.add(itemAboveSelected, index);
					EditorPriorityList.this.guiList.remove(index + 1);
					EditorPriorityList.this.guiList.select(index - 1);
				}
			}
		});
		
		this.btDown = new Button(buttonGroup, SWT.PUSH);
		this.btDown.setText("Down");
		this.btDown.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.btDown.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				super.widgetSelected(e);
				int index = EditorPriorityList.this.guiList.getSelectionIndex();
				if (index > -1 && index < EditorPriorityList.this.guiList.getItemCount() - 1) {
					String selectedItem = EditorPriorityList.this.guiList.getItem(index);  
					String itemBelowSelected = EditorPriorityList.this.guiList.getItem(index + 1);
					
					java.util.List<String> editors = EditorPriorityList.this.selectedEditors.get(EditorPriorityList.this.modelTypeCombo.getText());
					editors.add(index + 1, editors.remove(index));
					
					EditorPriorityList.this.guiList.remove(index + 1);
					EditorPriorityList.this.guiList.remove(index);
					EditorPriorityList.this.guiList.add(itemBelowSelected, index);
					EditorPriorityList.this.guiList.add(selectedItem, index + 1);	
					EditorPriorityList.this.guiList.select(index + 1);
				}				
			}
		});
		Composite addComposite = new Composite(this.container, SWT.NONE);
		layout  = new GridLayout(2, false);
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		addComposite.setLayout(layout);
		gdTitleLabel = new GridData(GridData.FILL_HORIZONTAL);
		gdTitleLabel.horizontalSpan = 2;		
		addComposite.setLayoutData(gdTitleLabel);
		
		/*noteLabel1 = new Label(addComposite, SWT.NONE);
		noteLabel1.setFont(new Font(addComposite.getDisplay(), "Arial", 9, SWT.BOLD)); 
		noteLabel1.setText("Note:");
		noteLabel2 = new Label(addComposite, SWT.NONE);
		noteLabel2.setText("If the model can't be opened using any of the above editors, the user will be asked for an editor to use.");*/
		selectionChanged();
	}

	@Override
	protected final void doLoad() {
		//CarismaGUI.INSTANCE.getPreferenceStore().setValue(getPreferenceName(), "");
		String savedProperties = CarismaGUI.INSTANCE.getPreferenceStore().getString(getPreferenceName());
		init(savedProperties);
		updatePriorityList();
	}

	@Override
	public final void load() {
		doLoad();
		//super.load();
	}
	
	@Override
	protected final void doLoadDefault() {
		//CarismaGUI.INSTANCE.getPreferenceStore().setValue(getPreferenceName(), "");
		//String savedProperties = getPreferenceStore().getDefaultString(getPreferenceName());
		init("");
		updatePriorityList();
	}

	@Override
	protected final void doStore() {
		java.util.List<String> toBeSaved = new java.util.ArrayList<>(); 
		Set<Map.Entry<String, java.util.ArrayList<String>>> set = this.selectedEditors.entrySet();
		for (Map.Entry<String, java.util.ArrayList<String>> entry : set) {
			for (String editor : entry.getValue()) {
				toBeSaved.add(entry.getKey() + ":" + editor);
			}
		}
		StringBuffer result = new StringBuffer();
		result.append(SELECTED_MODELTYPE + this.modelTypeCombo.getText() + SEPARATOR);
		result.append(LIST_ITEMS);
		for (int i = 0; i < toBeSaved.size(); i++) {
			result.append(toBeSaved.get(i));
			if (i + 1 < toBeSaved.size()) {
				result.append(SEPARATOR_LIST);
			}
		}	
		CarismaGUI.INSTANCE.getPreferenceStore().setValue(getPreferenceName(), result.toString());	
	}

	@Override
	public final void store() {
		doStore();
		//super.store();

	}

	@Override
	public final int getNumberOfControls() {

		return 1;
	}

	/**
	 * Initializes the internal data structure and the GUI elements.
	 * @param propertyString The saved properties represented as string
	 */
	private void init(final String propertyString) {
		this.listModelTypes = Carisma.getInstance().getModelTypeRegistry().getSupportedTypes();
		
		this.modelTypeCombo.removeAll();
		for (ModelType type : this.listModelTypes) {
			this.modelTypeCombo.add(type.getName());
		}
		
		this.selectedEditors = new HashMap<>();
		this.guiList.removeAll();
		for (ModelType mt : this.listModelTypes) {
			java.util.List<String> editors = getSavedEditorsByModelType(propertyString, mt.getName());
			if (editors.size() > 0) {
				this.selectedEditors.put(mt.getName(), (ArrayList<String>) editors);
			} else {
				this.selectedEditors.put(mt.getName(),
						new ArrayList<>(Arrays.asList(new String[] {TextEditorDescriptor.NAME})));
			}
		}

		initModeltypeCombo(propertyString);
	}

	/**
	 * Initializes the GUI element ModelTypeCombo.
	 * @param propertyString The saved properties represented as string
	 */
	private void initModeltypeCombo(final String propertyString) {
		String selectedModeltype = getProperty(propertyString, SELECTED_MODELTYPE);

		String[] comboItems = this.modelTypeCombo.getItems();
		int index = -1;
		for (int i = 0; i < comboItems.length; i++) {
			if (comboItems[i].equalsIgnoreCase(selectedModeltype)) {
				index = i;
			}
		}
		if (index != -1) {
			this.modelTypeCombo.select(index);
		} else {
			if (comboItems.length > 0) {
				this.modelTypeCombo.select(0);
			}
		}
	}

	/**
	 * Updates the priority list.
	 */
	void updatePriorityList() {
		this.guiList.removeAll();
		String selectedModeltype = this.modelTypeCombo.getText();
		java.util.List<String> items = this.selectedEditors.get(selectedModeltype);
		if (items != null) {
			for (String editor : items) {
				this.guiList.add(editor);
			}
		}
	}

	/**
	 * Opens a dialog to add an editor to the priority list.
	 * @return the result, or <code>null</code> if nothing chosen
	 */
	String openEditorSelectionDialog() {
		ListDialog dialog = new ListDialog(this.container.getShell());
		dialog.setContentProvider(ArrayContentProvider.getInstance());
		dialog.setLabelProvider(new LabelProvider());
		dialog.setTitle("Add editor for automatic opening");
		// TODO write Context help and reference it here

		java.util.List<String> input = getAvailableEditors();
		dialog.setInput(input);
		dialog.open();

		Object[] result = dialog.getResult();
		if (result != null && result.length > 0) {
			return (String) result[0];
		}

		return null;

	}
	
	/**
	 * Filters the available editors in regard to the selected model type.
	 * @return java.util.List<String> List of available editors represented as string object
	 */
	private java.util.List<String> getAvailableEditors() {
		java.util.List<String> choosenEditors = Arrays.asList(this.guiList.getItems());
		
		// Info: availableItems must be a copy of list. Otherwise, when removing items in list, they are no more there
		java.util.ArrayList<String> availableItems  = new ArrayList<>();
		
		java.util.List<EditorDescriptor> edDescriptors = CarismaGUI.INSTANCE.getEditorRegistry().getRegisteredEditors();
		for (EditorDescriptor edDesc : edDescriptors) {
			if (edDesc.isAvailable() 
					&& !choosenEditors.contains(edDesc.getName())
					&& (edDesc.getTypes().contains(".*") 
							|| edDesc.getTypes().contains(this.modelTypeCombo.getText().toLowerCase()) 
							|| (this.modelTypeCombo.getText().equalsIgnoreCase("uml2") ? edDesc.getTypes().contains("uml") 
									: this.modelTypeCombo.getText().equalsIgnoreCase("bpmn2") ? edDesc.getTypes().contains("bpmn") : false))) {
				availableItems.add(edDesc.getName());
			}
		}
		
		return availableItems;
	}

	/**
	 * Remove-/Up-/Down-Buttons are disabled, when listControl contains only one entry.
	 */
	void selectionChanged() {

		int count = this.guiList.getItemCount();
		if (count > 1) {
			this.btRemove.setEnabled(true);
			this.btUp.setEnabled(true);
			this.btDown.setEnabled(true);
			return;
		}
		this.btRemove.setEnabled(false);
		this.btUp.setEnabled(false);
		this.btDown.setEnabled(false);
		
	}
	
	/**
     * Set whether or not the controls in the field editor are enabled.
     * @param enabled The enabled state.
     * @param parent The parent of the controls in the group.
     *  Used to create the controls if required.
     */
	@Override
	public final void setEnabled(final boolean enabled, final Composite parent) {
		super.setEnabled(enabled, parent);
		this.btAdd.setEnabled(enabled);
		this.btRemove.setEnabled(enabled);
		this.btUp.setEnabled(enabled);
		this.btDown.setEnabled(enabled);
		
		this.modelTypeCombo.setEnabled(enabled);
		this.guiList.setEnabled(enabled);
		
		this.modelTypeLabel.setEnabled(enabled);
		this.titleLabel.setEnabled(enabled);
		/*noteLabel1.setEnabled(enabled);
		noteLabel2.setEnabled(enabled);*/
		
		if (enabled) {
			selectionChanged();
		}
	}
	
	/**
	 * Method extract a saved property. 
	 * @param savedString The saved property string
	 * @param propertyName The property name
	 * @return If available the desired property represented as string,
	 * 	otherwise an empty string.
	 */
	private static String getProperty(final String savedString, final String propertyName) {
		String[] properties = savedString.split(SEPARATOR);
		for (String prop : properties) {
			if (prop.length() > propertyName.length() 
					&& prop.substring(0, propertyName.length()).equalsIgnoreCase(propertyName)) {
				return prop.substring(propertyName.length());
			}
		}
		return "";
	}
	
	/**
	 * Method will look up the saved editors by model type.
	 * @param savedString The saved property string
	 * @param modelType The desired model type
	 * @return List of available editors represented as string object
	 */
	private static java.util.List<String> getSavedEditorsByModelType(final String savedString, final String modelType) {
		String[] tmp = getProperty(savedString, LIST_ITEMS).split(SEPARATOR_LIST);
		ArrayList<String> result =  new ArrayList<>();
		if (tmp.length != 0) {
			for (String str : tmp) {
				if (str.length() > modelType.length() + 1
						&& str.substring(0, modelType.length() + 1).equalsIgnoreCase(modelType + ":")) {
					result.add(str.substring(modelType.length() + 1));
				}
			}
			return result;
		}
		return result;
	}
	
	/**
	 * Method will look up the saved editors by model type.
	 * @param modelType The desired model type 
	 * @return List of available editors represented as string object
	 */
	public static java.util.List<String> getPriorityList(final String modelType) {
		String savedProperties = CarismaGUI.INSTANCE.getPreferenceStore().getString(Constants.EDITORS_LIST);
		return getSavedEditorsByModelType(savedProperties, modelType);
	}
}
