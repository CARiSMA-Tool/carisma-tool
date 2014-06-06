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
import org.eclipse.emf.edit.ui.dnd.LocalTransfer;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.fieldassist.ControlDecoration;
import org.eclipse.jface.fieldassist.FieldDecorationRegistry;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Sash;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.DetailsPart;
import org.eclipse.ui.forms.IManagedForm;
import org.eclipse.ui.forms.MasterDetailsBlock;
import org.eclipse.ui.forms.SectionPart;
import org.eclipse.ui.forms.widgets.FormToolkit;

import carisma.core.analysis.CheckReference;
import carisma.core.checks.CheckDescriptor;
import carisma.core.models.ModelTypeRegistry;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.editors.AdfEditorCheckSelectionDialog.CheckSelectionContentProvider;
import carisma.ui.eclipse.editors.AdfEditorCheckSelectionDialog.CheckSelectionLabelProvider;
import carisma.ui.eclipse.editors.descriptions.EditorDescriptor;
import carisma.ui.eclipse.preferences.Constants;
import carisma.ui.eclipse.preferences.pages.EditorPriorityList;

/**
 * Structure and behavior of the adf editor GUI.
 */
public class AdfEditorMasterDetailsBlock extends MasterDetailsBlock {

	/**
	 * The controller between model (analysis) and view (adf editor).
	 */
	private AdfEditorController controller;

	/**
	 * The open model button.
	 */
	private Button openModel;

	/**
	 * Combo menu for selected editor.
	 */
	private Combo selectedEditorCombo;

	/**
	 * The corresponding table viewer.
	 */
	private CheckboxTableViewer viewer;

	/**
	 * The del-button.
	 */
	private Button del;

	/**
	 * The up-button.
	 */
	private Button up;

	/**
	 * The down-button.
	 */
	private Button down;

	/**
	 * The run-button.
	 */
	private Button run;

	/**
	 * The show problems link.
	 */
	private Link problemLink;

	/**
	 * A boolean which indicates if the editor priority queue is used.
	 */
	private boolean selectedEditorPriorityListEnabled;

	/**
	 * A static name of the default editor selection.
	 */
	private static final String DEFAULT_EDITOR = "Default Eclipse Editor";

	/**
	 * A red cross as a decoration for the model file name text box.
	 */
	private ControlDecoration modelFileNameDecoration;

	/**
	 * A information sign as a decoration for the selected editor combo box.
	 */
	private ControlDecoration selectedEditorDecoration;

	/**
	 * The last selection in the list of checks.
	 */
	private ISelection lastSelection;

	/**
	 * A listener which is registered to every GUI element which has an
	 * influence on the validation of the analysis.
	 */
	private Listener masterListener;
	

	/**
	 * Constructor.
	 * 
	 * @param controller
	 *            the corresponding AdfEditorController instance
	 */
	public AdfEditorMasterDetailsBlock(final AdfEditorController controller) {
		this.controller = controller;
	}
	


	

	/**
	 * Constructs the whole GUI of the adf editor.
	 * 
	 * @param managedForm
	 *            managedForm for MasterDetailsBlock
	 * @param parent
	 *            the parent Composite
	 */
	protected final void createMasterPart(final IManagedForm managedForm,
			final Composite parent) {
		FormToolkit toolkit;
		controller.loadAnalysis();
		toolkit = managedForm.getToolkit();

		createMasterValidationModifyListener();

		// Master composite
		final Composite masterComposite = toolkit.createComposite(parent);
		GridLayout masterCompositeLayout = new GridLayout(3, false);
		masterCompositeLayout.marginHeight = 2;
		masterCompositeLayout.marginWidth = 10;
		masterComposite.setLayout(masterCompositeLayout);
		masterComposite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

		// Analysis name
		createAnalysisNameText(toolkit, masterComposite);

		// Model file name and browse button
		createModelfileArea(toolkit, masterComposite);

		// Selected editor combo box and open model button
		createSelectedEditorCombo(toolkit, masterComposite);
		createOpenModelButton(toolkit, masterComposite);

		// Create model type informations
		toolkit.createLabel(masterComposite, "Modeltype:");
		Label modeltypeLabel = toolkit.createLabel(masterComposite,
				controller.getModelType());
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalSpan = 2;
		gridData.horizontalIndent = 6;
		modeltypeLabel.setLayoutData(gridData);

		// Separator
		Composite separator = toolkit.createCompositeSeparator(masterComposite);
		gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
		gridData.horizontalSpan = 3;
		gridData.heightHint = 2;
		separator.setLayoutData(gridData);

		// Composite for selected check list and buttons
		Composite checkListComposite = toolkit.createComposite(masterComposite);
		GridLayout checkListCompositeLayout = new GridLayout(2, false);
		checkListCompositeLayout.marginHeight = 0;
		checkListCompositeLayout.marginWidth = 0;
		checkListComposite.setLayout(checkListCompositeLayout);
		GridData checkListCompositeGridData = new GridData(GridData.FILL_BOTH);
		checkListCompositeGridData.horizontalSpan = 3;
		checkListComposite.setLayoutData(checkListCompositeGridData);

		// Label 'selected checks'
		Label label = toolkit.createLabel(checkListComposite,
				"Selected checks:");
		gridData = new GridData();
		gridData.horizontalSpan = 2;
		label.setLayoutData(gridData);

		// Check list table and viewer
		createTableAndViewer(toolkit, checkListComposite, managedForm);

		// Create add-, delete-, up- and down-buttons and their listeners
		createAddButton(toolkit, checkListComposite);
		createDelButton(toolkit, checkListComposite);
		createUpButton(toolkit, checkListComposite);
		createDownButton(toolkit, checkListComposite);

		// Create Run-Button and the Listener
		createRunButton(toolkit, masterComposite);

		// Update availability of Up-, Down- and Run-Button
		listSelectionChanged();

		// Update enable state of GUI elements referring amongst other to editor
		// selection art
		CarismaGUI.INSTANCE.getPreferenceStore().firePropertyChangeEvent(
				Constants.EDITOR_SELECTION_ART,
				CarismaGUI.INSTANCE.getPreferenceStore().getString(
						Constants.EDITOR_SELECTION_ART),
				CarismaGUI.INSTANCE.getPreferenceStore().getString(
						Constants.EDITOR_SELECTION_ART));
		updateRunButtonEnable();
	}

	/**
	 * Creates the master listener.
	 */
	private void createMasterValidationModifyListener() {
		this.masterListener = new Listener() {
			@Override
			public void handleEvent(final Event event) {
				updateRunButtonEnable();
			}
		};
	}

	/**
	 * Creates the analysis name text and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding Composite
	 */
	private void createAnalysisNameText(final FormToolkit toolkit,
			final Composite composite) {
		toolkit.createLabel(composite, "Name:", SWT.LEFT);
		final Text textAnalysisName = toolkit.createText(composite,
				controller.getAnalysisName(), SWT.SINGLE);
		GridData gridData = new GridData(GridData.FILL_HORIZONTAL);
		gridData.horizontalSpan = 2;
		gridData.horizontalIndent = 6;
		textAnalysisName.setLayoutData(gridData);
		textAnalysisName.setEditable(true);
		textAnalysisName.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(final ModifyEvent e) {
				controller.setAnalysisName(textAnalysisName.getText());
			}
		});
	}

	/**
	 * Creates the model file label, text, the browse button and the
	 * corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding Composite
	 */
	private void createModelfileArea(final FormToolkit toolkit,
			final Composite composite) {
		toolkit.createLabel(composite, "Modelfile:");

		final Text modelFileName = toolkit.createText(composite, "");
		GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
		gridData.horizontalIndent = 6;
		modelFileName.setLayoutData(gridData);
		modelFileName.setEditable(false);
		final String modelfile = controller.getModelFile().toString();
		modelFileName.setText(modelfile);

		// Red cross if model does not exist
		modelFileNameDecoration = new ControlDecoration(modelFileName, SWT.LEFT
				| SWT.TOP);
		Image errorDecoration = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_ERROR)
				.getImage();
		modelFileNameDecoration.setImage(errorDecoration);
		modelFileNameDecoration.setShowHover(true);
		modelFileNameDecoration
				.setDescriptionText("The model file does not exist! Please\n"
						+ "recover the selected file or use the\n"
						+ "'Browse' button to choose a new one.");
		if (controller.isModelFileValid()) {
			modelFileNameDecoration.hide();
		} else {
			modelFileNameDecoration.show();
		}

		Button browse = toolkit.createButton(composite, "Browse...", SWT.PUSH);
		browse.setLayoutData(new GridData(SWT.FILL, SWT.TOP, false, false));
		browse.setToolTipText("Browse for a\n" + "new model file");

		browse.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				FileDialog fileDialog = new FileDialog(composite.getShell(),
						SWT.OPEN);
				fileDialog.setText("Choose a model file");

				ModelTypeRegistry modelTypeRegistry = CarismaGUI.INSTANCE
						.getModelTypeRegistry();
				String extensions = modelTypeRegistry.getModelTypeWithName(
						controller.getModelType()).getFileExtension();

				// Delete whitespaces and add '*.' in front of the extension to
				// set up
				// extension filter correctly
				int iterate = 0;
				String[] extensionArray = extensions.split(",");
				for (String singleExtension : extensionArray) {
					extensionArray[iterate++] = "*." + singleExtension.trim();
				}
				fileDialog.setFilterExtensions(extensionArray);

				// Determine initial path of file dialog
				String initialPath = controller.getModelIFile()
						.getRawLocation().toOSString();
				// Cut off file name from path
				if (initialPath.contains("\\")) {
					initialPath = initialPath.substring(0,
							initialPath.lastIndexOf('\\'));
				}
				if (initialPath.contains("/")) {
					initialPath = initialPath.substring(0,
							initialPath.lastIndexOf('/'));
				}
				fileDialog.setFilterPath(initialPath);

				String filepath = fileDialog.open();
				if (filepath != null) {
					try {
						IFile modelIFile = controller.getLinkedIFile(filepath);
						modelFileName.setText(modelIFile.getFullPath()
								.toString());
						controller.setModelIFile(modelIFile);
						updateRunButtonEnable();
						updateOpenModelButtonEnable();
					} catch (Exception exc) {
						exc.printStackTrace();
					}
				}
			}
		});
	}

	/**
	 * Creates the selected editor combo box.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding Composite
	 */
	private void createSelectedEditorCombo(final FormToolkit toolkit,
			final Composite composite) {
		toolkit.createLabel(composite, "Associated Editor:");

		selectedEditorCombo = new Combo(composite, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
		GridData gridData = new GridData(SWT.FILL, SWT.TOP, true, false);
		gridData.horizontalIndent = 6;
		selectedEditorCombo.setLayoutData(gridData);
		// The enable state will be updated later. Have to false for now
		// See updateGuiEnableState(..) for more information
		selectedEditorCombo.setEnabled(false);

		// Information icon if priority queue is used
		selectedEditorDecoration = new ControlDecoration(selectedEditorCombo,
				SWT.LEFT | SWT.TOP);
		Image questionDecoration = FieldDecorationRegistry.getDefault()
				.getFieldDecoration(FieldDecorationRegistry.DEC_INFORMATION)
				.getImage();
		selectedEditorDecoration.setImage(questionDecoration);
		selectedEditorDecoration.setShowHover(true);

		// Change Listener for the EDITOR_SELECTION_ART property
		final IPropertyChangeListener propertyChangeListener = new IPropertyChangeListener() {
			@Override
			public void propertyChange(final PropertyChangeEvent event) {
				if (event.getProperty().equals(Constants.EDITOR_SELECTION_ART)) {
					String editorSelectionArt = event.getNewValue().toString();
					if (editorSelectionArt.equals(Constants.AUTO)) {
						selectedEditorPriorityListEnabled = true;
					} else {
						selectedEditorPriorityListEnabled = false;
					}
					updateOpenModelButtonEnable();
				}
			}
		};
		CarismaGUI.INSTANCE.getPreferenceStore().addPropertyChangeListener(
				propertyChangeListener);

		// Selection Listener
		final EditorRegistry editorRegistry = CarismaGUI.INSTANCE
				.getEditorRegistry();
		selectedEditorCombo.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				String actualSelectedEditorName = selectedEditorCombo
						.getItem(selectedEditorCombo.getSelectionIndex());
				if (!actualSelectedEditorName.equals(DEFAULT_EDITOR)) {
					EditorDescriptor actualEditorDescriptor = editorRegistry
							.getEditorDescriptorByName(actualSelectedEditorName);
					controller.setSelectedEditorId(actualEditorDescriptor
							.getId());
				} else {
					controller.setSelectedEditorId(DEFAULT_EDITOR);
				}
			}

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
			}
		});

		// Add dispose listener to remove PropertyChangeListeners
		composite.getParent().addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(final DisposeEvent e) {
				CarismaGUI.INSTANCE.getPreferenceStore()
						.removePropertyChangeListener(propertyChangeListener);
			}
		});
	}

	/**
	 * Creates the open model button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding Composite
	 */
	private void createOpenModelButton(final FormToolkit toolkit,
			final Composite composite) {
		openModel = toolkit.createButton(composite, "Open Model", SWT.PUSH);
		openModel.setEnabled(controller.isModelFileValid());
		openModel.setToolTipText("Browse for a\n" + "new model file");

		openModel
				.setToolTipText("Open a model with an associated editor or an \n"
						+ "editor, defined in the editor priority list.\n"
						+ "Set CARiSMA's preferences to change the functionallity\n"
						+ "of this button. Select \"Editor selection combo box\" to\n"
						+ "save the choice in the analysis file.\n\n"
						+ "Read the Help article for more information.");

		openModel.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				String selectedEditorName = selectedEditorCombo
						.getItem(selectedEditorCombo.getSelectionIndex());
				controller.openModelEditor(selectedEditorName
						.equals(DEFAULT_EDITOR));
			}
		});
	}

	/**
	 * Creates the table viewer.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding Composite
	 * @param managedForm
	 *            for MasterDetailsBlock
	 */
	private void createTableAndViewer(final FormToolkit toolkit,
			final Composite composite, final IManagedForm managedForm) {
		Table table = toolkit.createTable(composite, SWT.CHECK);
		GridData gridData = new GridData(GridData.FILL_BOTH);
		gridData.heightHint = 20;
		gridData.widthHint = 100;
		gridData.verticalSpan = 4;
		table.setLayoutData(gridData);

		ISelectionChangedListener selectionChangeListener = new ISelectionChangedListener() {
			public void selectionChanged(final SelectionChangedEvent event) {
				if (listSelectionChanged()) {
					managedForm.fireSelectionChanged(new SectionPart(composite,
							toolkit, SWT.NONE), event.getSelection());
				}
			}
		};

		viewer = new CheckboxTableViewer(table);
		viewer.addSelectionChangedListener(selectionChangeListener);

		viewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(final DoubleClickEvent event) {
				Object selection = ((IStructuredSelection) viewer
						.getSelection()).getFirstElement();
				if (selection != null && selection instanceof CheckReference) {
					boolean state = viewer.getChecked(selection);
					state = state ^ true; // XOR swaps
					viewer.setChecked(selection, state);
					viewer.refresh();
					controller.setCheckSelection((CheckReference) selection,
							state);
					updateRunButtonEnable();
				}
			}
		});

		viewer.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(final CheckStateChangedEvent event) {
				Object obj = event.getElement();
				controller.setCheckSelection((CheckReference) obj,
						event.getChecked());
				listSelectionChanged();
				updateRunButtonEnable();
			}
		});

		viewer.getTable().addKeyListener(new KeyListener() {
			@Override
			public void keyPressed(final KeyEvent e) {
				if (e.keyCode == SWT.DEL) {
					deleteSelectedCheck();
				}
			}

			@Override
			public void keyReleased(final KeyEvent e) {
				// Do nothing
			}
		});

		viewer.setContentProvider(new SelectedChecksContentProvider());
		viewer.setLabelProvider(new SelectedChecksLabelProvider());
		viewer.setInput(controller.getSelectedChecksList());
		int dragAndDropOperations = DND.DROP_MOVE;
		Transfer[] transferTypes = new Transfer[] { LocalTransfer.getInstance() };
		viewer.addDragSupport(dragAndDropOperations, transferTypes,
				new SelectedChecksDragListener(viewer));
		viewer.addDropSupport(dragAndDropOperations, transferTypes,
				new SelectedChecksDropListener(viewer, controller));

		updateTable();
		viewer.getTable().select(0);
		//seems that selecting also works if there is no entry in the table
		//than nothing will be selected, but also no NullpointerException etc.
		initInvalidChecks();
	}

	/**
	 * Creates the add button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding composite
	 */
	private void createAddButton(final FormToolkit toolkit,
			final Composite composite) {
		Button add = toolkit.createButton(composite, "", SWT.PUSH);
		add.setImage(PlatformUI.getWorkbench().getSharedImages()
				.getImage(ISharedImages.IMG_OBJ_ADD));
		add.setToolTipText("Add checks\n" + "to the list");

		add.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				List<CheckDescriptor> fittingChecks = CarismaGUI.INSTANCE
						.getCheckRegistry().findChecks(
								controller.getModelType());

				AdfEditorCheckSelectionDialog dialog = new AdfEditorCheckSelectionDialog(
						composite.getShell(), fittingChecks,
						new CheckSelectionContentProvider(),
						new CheckSelectionLabelProvider(), "Choose from checks");

				List<CheckDescriptor> recChecks = CarismaGUI.INSTANCE
						.getCheckRegistry().getRecommendedChecks(
								controller.getModelIFile());
				if (recChecks != null && recChecks.size() > 0) {
					dialog.setRecommendedChecks(recChecks);
				}

				dialog.open();

				if (dialog.getResult() != null) {
					for (Object id : dialog.getResult()) {
						controller.createCheck(id.toString());
						viewer.refresh();
						updateTable();
						updateRunButtonEnable();
					}
				}
				listSelectionChanged();
			}
		});
	}

	/**
	 * Creates the delete button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding composite
	 */
	private void createDelButton(final FormToolkit toolkit,
			final Composite composite) {
		del = toolkit.createButton(composite, "", SWT.PUSH);
		del.setImage(PlatformUI.getWorkbench().getSharedImages()
				.getImage(ISharedImages.IMG_ETOOL_DELETE));
		del.setToolTipText("Delete the selected\n" + "check from the list");

		del.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				deleteSelectedCheck();
			}
		});
	}

	/**
	 * Creates the up button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding composite
	 */
	private void createUpButton(final FormToolkit toolkit,
			final Composite composite) {
		up = toolkit.createButton(composite, "", SWT.PUSH);
		up.setImage(CarismaGUI.INSTANCE.getImageRegistry().get(
				CarismaGUI.IMG_UP));
		up.setToolTipText("Move the check upwards");
		up.setEnabled(false);

		up.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				if (viewer.getTable().getSelectionIndex() > -1) {
					Object entryToBeMoved = viewer.getElementAt(viewer
							.getTable().getSelectionIndex());
					controller.removeCheck((CheckReference) entryToBeMoved);
					controller.getSelectedChecksList().add(
							viewer.getTable().getSelectionIndex() - 1,
							(CheckReference) entryToBeMoved);
				}
				viewer.refresh();
				listSelectionChanged();
			}
		});
	}

	/**
	 * Creates the up button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding composite
	 */
	private void createDownButton(final FormToolkit toolkit,
			final Composite composite) {
		down = toolkit.createButton(composite, "", SWT.PUSH);
		down.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		down.setImage(CarismaGUI.INSTANCE.getImageRegistry().get(
				CarismaGUI.IMG_DOWN));
		down.setToolTipText("Move the check downwards");
		down.setEnabled(false);

		down.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				if (viewer.getTable().getSelectionIndex() > -1) {
					Object entryToBeMoved = viewer.getElementAt(viewer
							.getTable().getSelectionIndex());
					controller.removeCheck((CheckReference) entryToBeMoved);
					controller.getSelectedChecksList().add(
							viewer.getTable().getSelectionIndex() + 1,
							(CheckReference) entryToBeMoved);
				}
				viewer.refresh();
				listSelectionChanged();
			}
		});
	}

	/**
	 * Creates the run-button and the corresponding listener.
	 * 
	 * @param toolkit
	 *            The FormToolKit where the button is created
	 * @param composite
	 *            The corresponding composite
	 */
	private void createRunButton(final FormToolkit toolkit,
			final Composite composite) {
		run = toolkit.createButton(composite, "RUN", SWT.PUSH);
		run.setEnabled(controller.isModelFileValid());
		//Bug #1518: Wie kann man die Position des Tooltips verändern?
		//http://stackoverflow.com/questions/11375250/set-tooltip-text-at-a-particular-location
		run.setToolTipText("Runs the analysis");

		run.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				listSelectionChanged();
				if (controller.isEditorDirty()) {
					MessageDialog messageDialog = new MessageDialog(composite
							.getShell(), "Save and Launch", null,
							"Do you want to save the changes?", 0,
							new String[] { "OK", "Cancel" }, 0);
					messageDialog.open();
					if (messageDialog.getReturnCode() == 0) {
						controller.saveAnalysis();
					}
				}
				if (controller.hasAnalysisInvalidParameters()
						|| !controller.isModelFileValid()
						|| !isOneCheckEnabled()) {
					updateRunButtonEnable();
					showProblems();
				} else {
					controller.runAnalysis();
				}
			}
		});

		problemLink = new Link(composite, SWT.NONE);
		problemLink.setText("<a>Show problems</a>");
		problemLink.setBackground(new Color(null, 255, 255, 255));
		problemLink.setVisible(false);
		problemLink.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(final SelectionEvent e) {
				showProblems();
			}
		});
	}

	/**
	 * Method checks for each check from the viewer if its enabled or disabled
	 * and set then an enabled/disabled check box on the left of each row. This
	 * method is called every time the table has to be drawn
	 */
	private void updateTable() {
		if (viewer != null) {
			if (viewer.getTable().getItemCount() > 0) {
				for (int i = 0; i < viewer.getTable().getItemCount(); i++) {
					Object obj = viewer.getElementAt(i);
					viewer.setChecked(obj, ((CheckReference) obj).isEnabled());
				}
			}
			viewer.refresh();
		}
	}

	/**
	 * Returns true, if the list selection is not equal to the last selection.
	 * This method also calls other methods to update the enable states of some
	 * GUI elements and resizes the detail part if necessary.
	 * 
	 * @return A boolean which indicates if the selection really changed
	 */
	private boolean listSelectionChanged() {
		updateListButtonsEnable();
		if (!viewer.getSelection().equals(lastSelection)) {
			lastSelection = viewer.getSelection();
			resizeDetailPart();
			return true;
		}
		return false;
	}

	/**
	 * Recalculates the ratio of the widths of the master and detail part.
	 */
	private void resizeDetailPart() {
		// The weights must be positive values and there must
		// be an entry for each non-sash child of the SashForm
		int nonSashCount = 0;
		for (Control control : this.sashForm.getChildren()) {
			if (!(control instanceof Sash)) {
				nonSashCount++;
			} else {
				Sash sash = (Sash) control;
				sash.setEnabled(false);
			}
		}
		int[] weights = new int[nonSashCount];
		for (int counter = 0; counter < nonSashCount; counter++) {
			weights[counter] = 1;
		}
		this.sashForm.setWeights(weights);
	}

	/**
	 * Check for invalid CheckReferences.
	 */
	private void initInvalidChecks() {
		List<CheckReference> uninputs = new ArrayList<CheckReference>();
		List<CheckReference> checks = controller.getSelectedChecksList();
		for (CheckReference checkReference : checks) {
			CheckDescriptor checkDescriptor = CarismaGUI.INSTANCE
					.getCheckRegistry().getCheckDescriptor(
							checkReference.getCheckID());
			if (checkDescriptor == null) {
				uninputs.add(checkReference);
			}
		}
		if (uninputs.size() > 0) {
			StringBuffer warning = new StringBuffer();
			if (uninputs.size() == 1) {
				viewer.getTable().setToolTipText(
						"Invalid CheckReference " + "\""
								+ uninputs.get(0).getCheckID() + "\"");
			} else {
				for (CheckReference cr : uninputs) {
					warning.append("\n\"");
					warning.append(cr.getCheckID());
					warning.append("\"");
				}
				viewer.getTable().setToolTipText(
						"Invalid CheckReferences: " + warning.toString());
			}
		}
	}

	/**
	 * Displays a message box which contains a list of problems.
	 */
	private void showProblems() {
		StringBuffer message = new StringBuffer("");

		for (String line : controller.getProblems()) {
			message.append(line + System.getProperty("line.separator"));
		}
		MessageDialog messageDialog = new MessageDialog(sashForm.getShell(),
				"Analysis problems", null, message.toString(),
				MessageDialog.ERROR, new String[] { "OK" }, 0);
		messageDialog.open();
	}

	/**
	 * Deletes the actual selected check from the list of checks.
	 */
	private void deleteSelectedCheck() {
		if (viewer.getTable().getSelectionIndex() > -1) {
			Object entryToBeDeleted = viewer.getElementAt(viewer.getTable()
					.getSelectionIndex());
			controller.removeCheck((CheckReference) entryToBeDeleted);
		}
		viewer.refresh();
		initInvalidChecks();
		listSelectionChanged();
		updateRunButtonEnable();
	}

	/**
	 * Returns true if at least one check is enabled.
	 * 
	 * @return true when at least one check is enabled
	 */
	private boolean isOneCheckEnabled() {
		List<CheckReference> checks = controller.getSelectedChecksList();
		int countEnabled = 0;
		for (CheckReference check : checks) {
			if (check.isEnabled()) {
				countEnabled++;
			}
		}
		return countEnabled == 0 ? false : true;
	}

	/**
	 * Disables the open model button if necessary.
	 */
	private void updateOpenModelButtonEnable() {
		// Is model file valid?
		if (controller.isModelFileValid()) {
			openModel.setEnabled(true);
			updateSelectedEditorEnableState(!selectedEditorPriorityListEnabled,
					"Editor priority list is used.\n"
							+ "Change this setting in the CARiSMA preferences.");
			modelFileNameDecoration.hide();
		} else {
			openModel.setEnabled(false);
			updateSelectedEditorEnableState(false,
					"The referenced model file is invalid");
			modelFileNameDecoration.show();			
		}
	}

	/**
	 * Disables the delete button if no check is selected. Disables the up and
	 * down button if they first or last check is selected.
	 */
	private void updateListButtonsEnable() {
		if (viewer.getSelection().isEmpty()) {
			del.setEnabled(false);
		} else {
			del.setEnabled(true);
			int selectionIndex = viewer.getTable().getSelectionIndex();
			if (selectionIndex > 0) {
				up.setEnabled(true);
			} else {
				up.setEnabled(false);
			}
			if (viewer.getTable().getSelectionIndex() < (viewer.getTable()
					.getItems().length - 1)) {
				down.setEnabled(true);
			} else {
				down.setEnabled(false);
			}
		}
	}

	/**
	 * Disabled the run button if the model file is not valid or the list of
	 * checks has no entry or if no check is enabled or if one of the parameters
	 * is invalid.
	 */
	private void updateRunButtonEnable() {
		boolean hasInvalidParameters = controller
				.hasAnalysisInvalidParameters();
		if (controller.isModelFileValid()) {
			controller.removeProblem(AdfEditorController.PROBLEM_MODEL);
			if (isOneCheckEnabled()) {
				controller.removeProblem(AdfEditorController.PROBLEM_CHECK);
				if (!hasInvalidParameters) {
					setEnableRunButton(true);
					controller.clearProblems();
				} else {
					setEnableRunButton(false);
				}
			} else {
				controller.addProblem(AdfEditorController.PROBLEM_CHECK,
						"The list contains no check or none is enabled");
				setEnableRunButton(false);
			}
		} else {
			controller.addProblem(AdfEditorController.PROBLEM_MODEL,
					"File is invalid");
			if (isOneCheckEnabled()) {
				controller.removeProblem(AdfEditorController.PROBLEM_CHECK);
			} else {
				controller.addProblem(AdfEditorController.PROBLEM_CHECK,
						"The list contains no check or none is enabled");
			}
			setEnableRunButton(false);
		}
	}

	/**
	 * Dis-/Enables the run button and shows the show problem link if necessary.
	 * 
	 * @param enableState
	 *            The new enable state of the run button
	 */
	private void setEnableRunButton(final boolean enableState) {
		run.setEnabled(enableState);
		problemLink.setVisible(!enableState);
	}

	/**
	 * This method activates or deactivates the editors combo box.
	 * 
	 * @param active
	 *            True, if the combo box should be activated, false otherwise
	 * @param message
	 *            the message, which will be shown
	 */
	private void updateSelectedEditorEnableState(final boolean active,
			final String message) {
		boolean validEditor = false;
		if (selectedEditorCombo.getEnabled() != active) {
			selectedEditorCombo.removeAll();
			if (active) {
				selectedEditorCombo.setEnabled(true);
				// Get the editors
				final EditorRegistry editorRegistry = CarismaGUI.INSTANCE
						.getEditorRegistry();
				List<EditorDescriptor> editorDescriptorList = editorRegistry
						.getRegisteredEditors();
				selectedEditorCombo.add(DEFAULT_EDITOR);
				// Add all applicable editors to the combo box
				for (EditorDescriptor editorDesc : editorDescriptorList) {
					if (editorDesc.isAvailable()
							&& editorDesc.isApplicable(controller
									.getModelIFile())) {
						selectedEditorCombo.add(editorDesc.getName());
					}
				}
				// Select 'Default Eclipse Editor' by default or the editor,
				// which is saved in the adf file
				selectedEditorCombo.select(0);
				EditorDescriptor savedEditorDescriptor = editorRegistry
						.getEditorDescriptorById(controller
								.getSelectedEditorId());
				if (savedEditorDescriptor != null) {
					int index = 0;
					for (String item : selectedEditorCombo.getItems()) {
						if (item.equals(savedEditorDescriptor.getName())) {
							selectedEditorCombo.select(index);
							validEditor = true;
							break;
						}
						index++;
					}
				} else {
					if ((controller.getSelectedEditorId().equals("")) || (controller.getSelectedEditorId().equals("Default Eclipse Editor"))) {
						validEditor = true;
					}
				}
				selectedEditorCombo
						.setToolTipText("Choose an editor to open the model.");
				selectedEditorDecoration.hide();
			} else {
				if (EditorPriorityList.getPriorityList(controller
						.getModelType()) != null
						&& EditorPriorityList.getPriorityList(
								controller.getModelType()).size() >= 1) {
					selectedEditorCombo.add(EditorPriorityList.getPriorityList(
							controller.getModelType()).get(0));
				} else {
					selectedEditorCombo.add("Priority list is empty!");
				}
				selectedEditorCombo.select(0);
				selectedEditorCombo.setEnabled(false);
				selectedEditorDecoration.setDescriptionText(message);
				selectedEditorCombo.setToolTipText("");
				selectedEditorDecoration.show();
			}
			if (!validEditor) {
				//MessageBox if the stored editor is invalid
				MessageDialog messageDialog = new MessageDialog(selectedEditorCombo.getShell(),
						"Invalid Model-Editor", null,
						"The chosen editor for the modeltype of the CARiSMA analysis is invalid."
						+ "\nThe ID is \"" + controller.getSelectedEditorId() + "\"."
						+ "\nThe default editor will be chosen."
						+ "\nClick \"OK && remove ID\" to change the editor to the default editor in the analysis file.",
						MessageDialog.INFORMATION,
						new String[] { "OK", "OK && remove ID" }, 0);
				// && in strings results in &
				messageDialog.open();
				if (messageDialog.getReturnCode() == 1) {
					//set the stored Editor to the default Editor and saves the analysis file
					setEditorToDefault();
				}	
			}
		}
	}
	
	/**
	 * Sets the editor for the modeltype to the eclipse default editor.
	 */
	private void setEditorToDefault() {
		String defaultEditor = "Default Eclipse Editor";
		controller.setSelectedEditorId(defaultEditor);
		controller.saveAnalysis();
	}

	@Override
	protected final void registerPages(final DetailsPart detailsPart) {
		AdfEditorCheckDetailsPage detailsPage = new AdfEditorCheckDetailsPage(
				this.controller, this.masterListener);
		detailsPart.registerPage(CheckReference.class, detailsPage);
	}

	@Override
	protected void createToolBarActions(final IManagedForm arg0) {
		// Auto-generated method stub
	}
	
}