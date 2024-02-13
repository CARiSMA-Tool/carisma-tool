/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *    
 * This class includes code fragments of <code>org.eclipse.ui.dialogs.ListSelectionDialog</code>.
 * @see ListSelectionDialog
 *******************************************************************************/
package carisma.ui.eclipse.editors;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.ListSelectionDialog;
import org.eclipse.ui.dialogs.SelectionDialog;

import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckDescriptorNameComparator;
import carisma.ui.eclipse.CarismaGUI;


/**
 * A dialog which provides the possibility to add selected checks to the analysis / adf file.
 */
public class AdfEditorCheckSelectionDialog extends SelectionDialog {

	/**
	 * List of recommended checks.
	 */
	List<CheckDescriptor> recommendedChecks = null;
	
	/**
	 * Label string.
	 */
	private static final String SELECT_ALL = "Select all";
	
	/**
	 * Label string.
	 */
	private static final String DESELECT_ALL = "Deselect all";

    /**
     * The root element to populate the viewer with.
     */
    private Object inputElements;

    /**
     * Providers for populating this dialog.
     */
    private ILabelProvider labelProvider;
    
    /**
     * Content provider given by calling class.
     */
    private IStructuredContentProvider contentProvider;
    
    /**
     * Button to select the recommended checks.
     */
    Button recommendedChecksButton = null;

    /**
     * The visual selection widget group.
     */
    CheckboxTableViewer listViewer;

    /**
     * Sizing constant.
     */
    private static final int SIZING_SELECTION_WIDGET_HEIGHT = 250;
    
    /**
     * Sizing constant.
     */
    private static final int SIZING_SELECTION_WIDGET_WIDTH = 300;

    /**
     * Constructor. Creates a list selection dialog.
     * 
     * @param parentShell the parent shell
     * @param input	the root element to populate this dialog with
     * @param contentProvider the content provider for navigating the model
     * @param labelProvider the label provider for displaying model elements
     * @param message the message to be displayed at the top of this dialog, or
     *    <code>null</code> to display a default message
     */
    public AdfEditorCheckSelectionDialog(final Shell parentShell,
    		final List<CheckDescriptor> input,
            final IStructuredContentProvider contentProvider,
            final ILabelProvider labelProvider, final String message) {
        super(parentShell);
        setTitle("Selection needed");
        this.inputElements = sortCheckListAlphabetically(input);
        this.contentProvider = contentProvider;
        this.labelProvider = labelProvider;
        if (message != null) {
			setMessage(message);
		} else {
			setMessage("Select the items"); 
		} 
    }
   
    /**
     * Add the selection and deselection buttons to the dialog.
     * 
     * @param composite org.eclipse.swt.widgets.Composite
     */
    private void addSelectionButtons(final Composite composite) {
        Composite buttonComposite = new Composite(composite, SWT.NONE);
        GridLayout layout = new GridLayout();
        layout.numColumns = 3;  
		layout.marginWidth = 0;
		layout.horizontalSpacing = convertHorizontalDLUsToPixels(IDialogConstants.HORIZONTAL_SPACING);
        buttonComposite.setLayout(layout);
        buttonComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

		this.recommendedChecksButton = new Button(buttonComposite, SWT.CHECK);
		this.recommendedChecksButton.setLayoutData(new GridData(SWT.LEFT));
		this.recommendedChecksButton.setText("Select recommended checks");
		//setRecommendedChecks(recommendedChecks, true);	// remove comment to initially add the checked Checks to the recommendChecks-List
		this.recommendedChecksButton.setSelection(false);		// set this on true to initially check the recommendedChecks
		
		this.recommendedChecksButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (AdfEditorCheckSelectionDialog.this.recommendedChecks != null) {
					if (AdfEditorCheckSelectionDialog.this.recommendedChecksButton.getSelection()) {
						setRecommendedChecks(AdfEditorCheckSelectionDialog.this.recommendedChecks, true);
					} else {
						setRecommendedChecks(AdfEditorCheckSelectionDialog.this.recommendedChecks, false);
					}
				}
			}
		});
        
        Button selectButton = createButton(buttonComposite,
                IDialogConstants.SELECT_ALL_ID, SELECT_ALL, false); 
 
        SelectionListener listener = new SelectionAdapter() {
            @Override
			public void widgetSelected(final SelectionEvent e) {
                AdfEditorCheckSelectionDialog.this.listViewer.setAllChecked(true);
            }
        };
        selectButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL, GridData.VERTICAL_ALIGN_FILL, false, false));
        selectButton.addSelectionListener(listener);

        Button deselectButton = createButton(buttonComposite,
                IDialogConstants.DESELECT_ALL_ID, DESELECT_ALL, false);
        deselectButton.setLayoutData(new GridData(GridData.HORIZONTAL_ALIGN_FILL, GridData.VERTICAL_ALIGN_FILL, false, false));
        
        listener = new SelectionAdapter() {
            @Override
			public void widgetSelected(final SelectionEvent e) {
                AdfEditorCheckSelectionDialog.this.listViewer.setAllChecked(false);
            }
        };
        deselectButton.addSelectionListener(listener);
    }

    /**
     * Visually checks the previously-specified elements in this dialog's list 
     * viewer.
     */
    private void checkInitialSelections() {
    	for (Object obj : getInitialElementSelections()) {
			this.listViewer.setChecked(obj, true);    		
    	}
    }
    
    /**
     * Sets the shell of the super class to the parameter shell.
     * 
     * @param shell Shell
     */
    @Override
	protected final void configureShell(final Shell shell) {
        super.configureShell(shell);
        PlatformUI.getWorkbench().getHelpSystem().setHelp(shell, CarismaGUI.PLUGIN_ID + ".Checks");
    }

    /**
     * Creates the dialog area.
     * 
     * @param parent the Composite
     * @return Control
     */
    @Override
	protected final Control createDialogArea(final Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);
        parent.getShell().setMinimumSize(450, 450);
        
        initializeDialogUnits(composite);
        
        createMessageArea(composite);

        this.listViewer = CheckboxTableViewer.newCheckList(composite, SWT.BORDER);
        GridData data = new GridData(GridData.FILL_BOTH);
        data.heightHint = SIZING_SELECTION_WIDGET_HEIGHT;
        data.widthHint = SIZING_SELECTION_WIDGET_WIDTH;
        this.listViewer.getTable().setLayoutData(data);

        this.listViewer.setLabelProvider(this.labelProvider);
        this.listViewer.setContentProvider(this.contentProvider);

        addSelectionButtons(composite);

        initializeViewer();
        
        if (this.recommendedChecksButton.getSelection()
        		&& (this.recommendedChecks != null 
        		&& this.recommendedChecks.size() > 0)) {
	        setInitialSelections(this.recommendedChecks.toArray());
        }
        if (!getInitialElementSelections().isEmpty()) {
			checkInitialSelections();
		}
        Dialog.applyDialogFont(composite);
        
        this.listViewer.addDoubleClickListener(new IDoubleClickListener() {
			@Override
			public void doubleClick(final DoubleClickEvent event) {
				Object selection = ((IStructuredSelection) AdfEditorCheckSelectionDialog.this.listViewer.getSelection()).getFirstElement();
				if (selection != null) {
					boolean state = AdfEditorCheckSelectionDialog.this.listViewer.getChecked(selection);
					state = state ^ true; // XOR swaps
					AdfEditorCheckSelectionDialog.this.listViewer.setChecked(selection, state);
					AdfEditorCheckSelectionDialog.this.listViewer.refresh();
				}
			}
		});
        
        return composite;
    }

    /**
     * Returns the viewer used to show the list.
     * 
     * @return the viewer, or <code>null</code> if not yet created
     */
    protected final CheckboxTableViewer getViewer() {
        return this.listViewer;
    }

    /**
     * Initializes this dialog's viewer after it has been laid out.
     */
    private void initializeViewer() {
        this.listViewer.setInput(this.inputElements);
    }

    /**
     * The <code>ListSelectionDialog</code> implementation of this 
     * <code>Dialog</code> method builds a list of the selected elements for later
     * retrieval by the client and closes this dialog.
     */
    @Override
	protected final void okPressed() {
    	
        // Get the input children.
        Object[] children = this.contentProvider.getElements(this.inputElements);

        // Build a list of selected children.
        if (children != null) {
            ArrayList<CheckDescriptor> list = new ArrayList<>();
            for (int i = 0; i < children.length; ++i) {
                Object element = children[i];
                if (this.listViewer.getChecked(element)) {
					list.add((CheckDescriptor) element);
				}
            }
            setResult(list);
        }

        super.okPressed();
    }
    
    /**
     * Nested Class: Content Provider.
     */
    static class CheckSelectionContentProvider implements ITreeContentProvider {

		@Override
		public void dispose() {
			//TODO: Why empty?
		}

		@Override
		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
			//TODO: Why empty?
		}

		@Override
		public Object[] getChildren(final Object parentElement) {
			return null;
		}

		@Override
		public Object getParent(final Object element) {
			return null;
		}

		@Override
		public boolean hasChildren(final Object element) {
			return false;
		}

		@Override
		public Object[] getElements(final Object inputElement) {
						
			if ((inputElement != null) 
					&& (inputElement instanceof Object[])) {
				return (Object[]) inputElement;
			}
			
			Object[] inputs = new Object[CarismaGUI.getCheckRegistry()
					.getRegisteredChecks().size()];
			for (int i = 0; i < CarismaGUI.getCheckRegistry()
					.getRegisteredChecks().size(); i++) {
				inputs[i] = CarismaGUI.getCheckRegistry()
						.getRegisteredChecks().get(i);
			}
			return inputs;
			
		}
	}

    /**
     * Nested Class: Label Provider.
     */
	static class CheckSelectionLabelProvider implements ILabelProvider {
		
		/**
		 * Returns a column text.
		 * 
		 * @param obj the object 
		 * @param index the index
		 * @return String the object
		 */
		public static String getColumnText(final Object obj, final int index) {
			return obj.toString();
		}
		
		/**
		 * Returns a column image.
		 * 
		 * @param obj the object
		 * @param index the index
		 * @return the Image
		 */
		public static Image getColumnImage(final Object obj, final int index) {
			if (obj instanceof CheckDescriptor) {
				return PlatformUI.getWorkbench().getSharedImages()
						.getImage(ISharedImages.IMG_OBJ_ELEMENT);
			}
			return PlatformUI.getWorkbench().getSharedImages()
					.getImage(ISharedImages.IMG_OBJ_FILE);
		}

		@Override
		public void addListener(final ILabelProviderListener listener) {
			//TODO: Why empty?
		}

		@Override
		public void dispose() {
			//TODO: Why empty?
		}

		@Override
		public boolean isLabelProperty(final Object element, final String property) {
			return false;
		}

		@Override
		public void removeListener(final ILabelProviderListener listener) {
			//TODO: Why empty?
		}

		@Override
		public Image getImage(final Object element) {
			if (element instanceof CheckDescriptor) {
				return PlatformUI.getWorkbench().getSharedImages()
						.getImage(ISharedImages.IMG_OBJ_ELEMENT);
			}
			return PlatformUI.getWorkbench().getSharedImages()
					.getImage(ISharedImages.IMG_OBJ_FILE);
		}

		@Override
		public String getText(final Object element) {
			return ((CheckDescriptor) element).getName();
		}
	}

	/**
	 * Fills the list of recommended checks with the given array of checks.
	 * 
	 * @param newRecommendedChecks an object array of recommended checks
	 */
	public final void setRecommendedChecks(final List<CheckDescriptor> newRecommendedChecks) {
		if (this.recommendedChecks != null) {
			this.recommendedChecks.clear();
		} else {
			this.recommendedChecks = new ArrayList<>();
		}
		this.recommendedChecks.addAll(newRecommendedChecks);
	}
	
	/**
	 * Changes the check state of the recommended checks in the list of chosen checks.
	 * 
	 * @param recommendedChecks an object array of recommended checks
	 * @param checked boolen
	 */
	protected final void setRecommendedChecks(final List<CheckDescriptor> recommendedChecks, final boolean checked) {
		if (recommendedChecks != null) {
			for (CheckDescriptor descriptor : recommendedChecks) {
				getViewer().setChecked(descriptor, checked);
			} 
		}
	}
	
	/**
	 * Sorts the input List of CheckDescriptor alphabetically.
	 * 
	 * @param checkList the unsorted list of checks
	 * @return Returns a list of checks which is sorted alphabetically
	 */
	private static Object[] sortCheckListAlphabetically(final List<CheckDescriptor> checkList) {
		Object[] returnObject = new Object[checkList.size()];
		Comparator<CheckDescriptor> comparator = new CheckDescriptorNameComparator();
		Collections.sort(checkList, comparator);
		for (int i = 0; i < checkList.size(); i++) {
			returnObject[i] = checkList.get(i);
		}
		return returnObject;
	}
	
}
