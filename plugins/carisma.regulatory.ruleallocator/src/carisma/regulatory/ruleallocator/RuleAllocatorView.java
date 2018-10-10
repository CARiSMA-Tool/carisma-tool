package carisma.regulatory.ruleallocator;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.ParserConfigurationException;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.xml.sax.SAXException;

import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.Situation;

/**
 * 'Rule Allocator' view which merges the former 'Allocation View', 'Rule View' and 'BPMN View'.
 * @author jkowald
 *
 */
public class RuleAllocatorView extends ViewPart implements ISelectionListener {
	
	/**
	 * String view id.
	 */
	public static String ID = "carisma.regulatory.ruleallocator.ruleallocatorview";
	
	/**
	 * The ListViewer of the RuleElements list.
	 */
	private ListViewer ruleElementListViewer;
	
	/**
	 * A list of objects which contains RuleElements and Situations.
	 */
	private ArrayList<Object> ruleElementList;
	
	/**
	 * A boolean which indicates if the list of RuleElements is loaded.
	 */
	private boolean ruleElementListLoaded;
	
	/**
	 * The ListViewer for the list of Allocations.
	 */
	private ListViewer allocationListViewer;
	
	/**
	 * A list of objects which are Allocations.
	 */
	private ArrayList<Object> allocationList;
	
	/**
	 * The ListViewer for the list of BPMNElements.
	 */
	private ListViewer bpmnElementListViewer;
	
	/**
	 * A list of objects which are BPMNElements.
	 */
	private ArrayList<Object> bpmnElementList;
	
	/**
	 * A boolean which indicates if the list of BPMNElements is loaded.
	 */
	private boolean bpmnElementListLoaded;
	
	/**
	 * The FormToolkit.
	 */
	private FormToolkit toolkit;
	
	/**
	 * The 'Allocate chosen items' button.
	 */
	private Button allocateButton;
	
	/**
	 * The 'Remove chosen allocation' button.
	 */
	private Button removeAllocationButton;
	
	/**
	 * The "Select rel. elements" button.
	 */
	private Button selectRelatedElements;
	
	/**
	 * The "Get Yaoqiang" button.
	 */
	private Button getYaoqiangSelection;
	
	/**
	 * The {@link Shell}.
	 */
	private Shell shell;
	
	/**
	 * Instance of the new DatamodelManager to load the data from the given file.
	 */
	private DatamodelManager datamodelManager;
	
	/**
	 * Instance of the {@link AllocationControllerOLD}.
	 */
	private AllocationController allocationController;
	
	/**
	 * The absolute path of the actual yaoqiang tempfile.
	 */
	private String yaoqiangTempfilePath;
	
	/**
	 * The timestamp of the actual yaoqiang tempfile.
	 */
	private long yaoqiangTempfileTimestamp;
	
	/**
	 * The BPMNElement which is selected in the Yaoqiang editor.
	 */
	private BPMNElement yaoqiangSelection;
	
	/**
	 * Constructor.
	 */
	public RuleAllocatorView() {
		ruleElementList = new ArrayList<Object>();
		allocationList = new ArrayList<Object>();
		bpmnElementList = new ArrayList<Object>();
		ruleElementList.add("Doubleclick here to load a RuleElements file ...");
		bpmnElementList.add("Doubleclick here to load a BPMN file ...");
		ruleElementListLoaded = false;
		bpmnElementListLoaded = false;
		datamodelManager = new DatamodelManager();
		allocationController = new AllocationController(datamodelManager);
		yaoqiangTempfilePath = "";
		yaoqiangTempfileTimestamp = 0;
		yaoqiangSelection = null;
	}
	
	@Override
	public void createPartControl(Composite parent) {
		Display display = parent.getDisplay();
		this.toolkit = new FormToolkit(display);
		this.shell = parent.getShell();
		
		createShellShutdownEvent();
		
		// The basic GridLayout & -Data
		GridLayout layoutParent = new GridLayout(1, true);
		layoutParent.horizontalSpacing = 5;
		layoutParent.verticalSpacing = 5;
		parent.setLayout(layoutParent);
		GridData dataParent = new GridData(SWT.FILL, SWT.FILL, true, true);
		parent.setLayoutData(dataParent);
		
		// The RuleElement list
		Label labelRuleElementList = new Label(parent, SWT.LEFT); 
		labelRuleElementList.setText("List of RuleElements:");
		createRuleElementArea(parent);

		// The allocation area
		Label labelAllocation = new Label(parent, SWT.LEFT); 
		labelAllocation.setText("Allocation:");
		createAllocationArea(parent);
		
		// The BPMN element list
		Label labelBPMNList = new Label(parent, SWT.LEFT); 
		labelBPMNList.setText("List of BPMN Elements:");
		createBPMNElementArea(parent);
	}

	@Override
	public void setFocus() {
		checkYaoqiangTempFile();
		this.ruleElementListViewer.refresh();
		this.allocationListViewer.refresh();
		this.bpmnElementListViewer.refresh();
	}
	
	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection selection) {
	}
	
	/**
	 * Creates the RuleElement list.
	 * @param parent The parent composite
	 */
	private void createRuleElementArea(final Composite parent) {
		this.ruleElementListViewer = new ListViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		GridData ruleElementListData = new GridData(
				GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL
				| GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
		ruleElementListViewer.getList().setLayoutData(ruleElementListData);
		
		this.ruleElementListViewer.setContentProvider(new ContentProvider());
		this.ruleElementListViewer.setLabelProvider(new RuleElementListLabelProvider());
		this.ruleElementListViewer.setInput(this.ruleElementList);
		
		this.ruleElementListViewer.addDoubleClickListener(new IDoubleClickListener() {
		    @Override
		    public void doubleClick(DoubleClickEvent event) {
		        if (!ruleElementListLoaded) {
		        	Shell shell = parent.getShell();
		        	FileDialog fileDialog = new FileDialog(shell);
		        	fileDialog.setFilterExtensions(new String[]{"*.xmi"});
		        	String filepath = fileDialog.open();
		        	if (filepath != null && filepath != "") {
		        		loadRuleElementFile(filepath);
		        	}
		        }
		    }
		});
		
		this.ruleElementListViewer.addSelectionChangedListener(new ISelectionChangedListener(){
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				allocateButton.setEnabled(false);
				Object selectedRuleElement = ((IStructuredSelection)ruleElementListViewer.getSelection()).getFirstElement();
				Object selectedBPMNElement = ((IStructuredSelection)bpmnElementListViewer.getSelection()).getFirstElement();
				if (selectedRuleElement != null && selectedBPMNElement != null) {
					if (selectedRuleElement instanceof RuleElement
							&& selectedBPMNElement instanceof BPMNElement) {
						boolean typeCheckOk = allocationController.checkTypes(
								(RuleElement) selectedRuleElement, (BPMNElement) selectedBPMNElement);							
						allocateButton.setEnabled(typeCheckOk);
					}
				}
			}
		});
	}
	
	/**
	 * Creates the allocation buttons and list.
	 * @param parent The parent composite
	 */
	private void createAllocationArea(Composite parent) {
		// Subcomposite to order the elements
		Composite allocationComp = new Composite(
				parent, 
				SWT.EMBEDDED | SWT.FILL | SWT.RIGHT);
		GridLayout allocationCompLayout = new GridLayout(2, false);
		GridData allocationCompData = new GridData(
				GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL);
		allocationComp.setLayout(allocationCompLayout);
		allocationComp.setLayoutData(allocationCompData);
		
		// 'Allocate chosen items' button
		this.allocateButton = this.toolkit.createButton(
				allocationComp, 
				"Allocate chosen items", 
				SWT.PUSH | SWT.FILL);
		this.allocateButton.setEnabled(false);
		this.allocateButton.setLayoutData(
				new GridData(SWT.FILL, SWT.BOTTOM, false, true));
		
		this.allocateButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				Object selectedRuleElement = ((IStructuredSelection)ruleElementListViewer.getSelection()).getFirstElement();
				Object selectedBPMNElement = ((IStructuredSelection)bpmnElementListViewer.getSelection()).getFirstElement();
				if (selectedRuleElement != null && selectedBPMNElement != null) {
					if (selectedRuleElement instanceof RuleElement
							&& selectedBPMNElement instanceof BPMNElement) {
						Allocation newAllocation = 
								allocationController.allocate((RuleElement) selectedRuleElement, (BPMNElement) selectedBPMNElement);
						if (newAllocation != null) {
							allocationList.add(newAllocation);
							allocationListViewer.refresh(true);
						} else {
							MessageDialog.openWarning(shell, "Warning", "Allocation already exist");
						}
					}
				}
			}
		});
		
		// Allocation list
		this.allocationListViewer = new ListViewer(
				allocationComp, 
				SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		GridData allocationListData = new GridData(
				GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL
				| GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
		allocationListData.verticalSpan = 4;
		this.allocationListViewer.getList().setLayoutData(allocationListData);
		this.allocationListViewer.setContentProvider(new ContentProvider());
		this.allocationListViewer.setLabelProvider(new AllocationListLabelProvider());
		this.allocationListViewer.setInput(this.allocationList);
		this.allocationListViewer.addSelectionChangedListener(new ISelectionChangedListener(){
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection select = (IStructuredSelection) allocationListViewer.getSelection();
				if (select != null && select.getFirstElement() instanceof Allocation) {
					removeAllocationButton.setEnabled(true);
					selectRelatedElements.setEnabled(true);
				} else {
					removeAllocationButton.setEnabled(false);
					selectRelatedElements.setEnabled(false);
				}
			}
		});
		
		// 'Remove chosen allocation' button
		this.removeAllocationButton = this.toolkit.createButton(
				allocationComp,
				"Remove chosen allocation",
				SWT.PUSH | SWT.FILL);
		this.removeAllocationButton.setEnabled(false);
		this.removeAllocationButton.setLayoutData(
				new GridData(SWT.LEFT, SWT.TOP, false, true));
		this.removeAllocationButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				IStructuredSelection select = (IStructuredSelection) allocationListViewer.getSelection();
				if (select != null && select.getFirstElement() instanceof Allocation) {
					allocationController.removeAllocation((Allocation) select.getFirstElement());
					allocationList.remove(select.getFirstElement());
					allocationListViewer.setSelection(null);
					allocationListViewer.refresh();
					removeAllocationButton.setEnabled(false);
				}
			}
		});
		
		// 'Select rel. Elements' button
		this.selectRelatedElements = this.toolkit.createButton(
				allocationComp,
				"Select relative elements",
				SWT.PUSH | SWT.FILL);
		this.selectRelatedElements.setEnabled(false);
		this.selectRelatedElements.setLayoutData(
				new GridData(SWT.FILL, SWT.TOP, false, true));
		this.selectRelatedElements.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				IStructuredSelection select = (IStructuredSelection) allocationListViewer.getSelection();
				if (select != null && select.getFirstElement() instanceof Allocation) {
					Allocation allocation = (Allocation) select.getFirstElement();
					int ruleElementIndex = ruleElementList.indexOf(allocation.getRuleElement());
					ruleElementListViewer.getList().setSelection(ruleElementIndex);
					ruleElementListViewer.refresh(true);
					BPMNElement chosenBPMNElement = findEqualInBPMNElementList(allocation.getBpmnElement());
					if (chosenBPMNElement != null) {
						int bpmnElementIndex = bpmnElementList.indexOf(chosenBPMNElement);
						bpmnElementListViewer.getList().setSelection(bpmnElementIndex);
						bpmnElementListViewer.refresh(true);
					}
				}
			}
		});
		
		// 'Get Yaoqiang selection' button
		this.getYaoqiangSelection = this.toolkit.createButton(
				allocationComp,
				"Get Yaoqiang selection",
				SWT.PUSH | SWT.FILL);
		this.getYaoqiangSelection.setLayoutData(
				new GridData(SWT.FILL, SWT.TOP, false, true));
		this.getYaoqiangSelection.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				checkYaoqiangTempFile();
			}
		});
	}
	
	/**
	 * Creates the BPMN element list.
	 * @param parent The parent composite
	 */
	private void createBPMNElementArea(final Composite parent) {
		this.bpmnElementListViewer = new ListViewer(parent, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		GridData bpmnElementListData = new GridData(
				GridData.HORIZONTAL_ALIGN_FILL | GridData.VERTICAL_ALIGN_FILL
				| GridData.GRAB_HORIZONTAL | GridData.GRAB_VERTICAL);
		bpmnElementListViewer.getList().setLayoutData(bpmnElementListData);
		
		this.bpmnElementListViewer.setContentProvider(new ContentProvider());
		this.bpmnElementListViewer.setLabelProvider(new BPMNElementListLabelProvider());
		this.bpmnElementListViewer.setInput(this.bpmnElementList);
		
		this.bpmnElementListViewer.addDoubleClickListener(new IDoubleClickListener() {
		    @Override
		    public void doubleClick(DoubleClickEvent event) {
		        if (!bpmnElementListLoaded) {
		        	Shell shell = parent.getShell();
		        	FileDialog fileDialog = new FileDialog(shell);
		        	fileDialog.setFilterExtensions(new String[]{"*.bpmn*"});
		        	String filepath = fileDialog.open();
		        	if (filepath != null && filepath != "") {
		        		loadBPMNFile(filepath);
		        	}
		        }
		    }
		});
		
		this.bpmnElementListViewer.addSelectionChangedListener(new ISelectionChangedListener(){
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				allocateButton.setEnabled(false);
				Object selectedRuleElement = ((IStructuredSelection)ruleElementListViewer.getSelection()).getFirstElement();
				Object selectedBPMNElement = ((IStructuredSelection)bpmnElementListViewer.getSelection()).getFirstElement();
				if (selectedRuleElement != null && selectedBPMNElement != null) {
					if (selectedRuleElement instanceof RuleElement
							&& selectedBPMNElement instanceof BPMNElement) {
						boolean typeCheckOk = allocationController.checkTypes(
								(RuleElement) selectedRuleElement, (BPMNElement) selectedBPMNElement);							
						allocateButton.setEnabled(typeCheckOk);
					}
				}
			}
		});
	}
	
	/**
	 * This method is called when the shell disposes.
	 * @param shell The shell which will dispose
	 */
	private void createShellShutdownEvent() {
		this.shell.addDisposeListener(new DisposeListener() {
			@Override
			public void widgetDisposed(final DisposeEvent disposeEvent) {
				datamodelManager.deleteYaoqiangTempfile(yaoqiangTempfilePath);
			}
		});
	}
	
	/**
	 * Returns the shell to classes in the same package.
	 * return the shell.
	 */
	protected Shell getShell() {
		return this.shell;
	}
	
	/**
	 * Loads a RuleElement xmi file.
	 * @param path
	 */
	protected void loadRuleElementFile(String path) {
		// Clear list
		this.ruleElementList.clear();
		this.allocationList.clear();
		
		boolean success = datamodelManager.loadFile(path);
		if (success) {
			Container container = datamodelManager.getContainer();
			
			// Add new items to the list
			for (Object ruleElementObject : container.getContainsRuleElement()) {
				if (ruleElementObject instanceof RuleElement) {
					this.ruleElementList.add(ruleElementObject);
				}
			}
			for (Object situationObject : container.getContainsSituation()) {
				if (situationObject instanceof Situation) {
					this.ruleElementList.add(situationObject);
				}
			}
			for (Object allocationObject : container.getContainsAllocation())	{
				if (allocationObject instanceof Allocation) {
					this.allocationList.add(allocationObject);
				}
			}
			
			// Refresh the ListViewers
			this.ruleElementListViewer.refresh(true);
			this.ruleElementListLoaded = true;
			this.allocationListViewer.refresh(true);
		} else {
			MessageDialog.openWarning(shell, "Warning", "Path '" + path + "' is not valid.");
		}
	}
		
	/**
	 * This method loads the BPMN file via a BPMNReader instance.
	 * @throws IOException
	 * @throws SAXException 
	 * @throws ParserConfigurationException 
	 * 
	 */
	protected void loadBPMNFile(String path) {
		setYaoqiangTempfilePath(path);
		this.bpmnElementList.clear();
		try {
			BPMNReader bpmnReader = new BPMNReader();
			bpmnReader.setFile(path);
			this.bpmnElementList.addAll(bpmnReader.readXMLFile());
			this.bpmnElementListLoaded = true;
		} catch (Exception e) {
			this.bpmnElementList.add("An error has occurred: " + e.getMessage());
			e.printStackTrace();
			this.bpmnElementList.add("Doubleclick here to load a BPMN file ...");
			this.bpmnElementListLoaded = false;
		}
		// Refresh the ListViewer
		this.bpmnElementListViewer.refresh(true);
	}
	
	/**
	 * This method sets the Yaoqiang tempfile path by modifying the BPMN file path.
	 * @param bpmnfilepath The absolute path to the loaded BPMN file
	 */
	private void setYaoqiangTempfilePath(String bpmnfilepath) {
		if (!this.yaoqiangTempfilePath.isEmpty()) {
			datamodelManager.deleteYaoqiangTempfile(yaoqiangTempfilePath);
		}
		int indexOfExtension = bpmnfilepath.lastIndexOf(".");
		String absolutePathWithoutExt = bpmnfilepath.substring(0, indexOfExtension);
		this.yaoqiangTempfilePath = absolutePathWithoutExt + "_chosen.temp";
		File newTempfile = datamodelManager.createYaoqiangTempfile(yaoqiangTempfilePath);
		yaoqiangTempfileTimestamp = newTempfile.lastModified();
		yaoqiangSelection = null;
	}
	
	/**
	 * This method checks the Yaoqiang temp file for changes.
	 */
	private void checkYaoqiangTempFile() {
		if (!yaoqiangTempfilePath.isEmpty()) {
			File tempFile = new File(yaoqiangTempfilePath);
			if (tempFile.exists()) {
				if (!(yaoqiangTempfileTimestamp == tempFile.lastModified())) {
					// If the file has been updated
					yaoqiangTempfileTimestamp = tempFile.lastModified();
					BPMNElement chosenBPMNElement = datamodelManager.readYaoqiangTempfile(yaoqiangTempfilePath);
					if (chosenBPMNElement != null) {
						BPMNElement listBPMNElement = findEqualInBPMNElementList(chosenBPMNElement);
						if (listBPMNElement != null) {
							yaoqiangSelection = listBPMNElement;
							int bpmnElementIndex = bpmnElementList.indexOf(listBPMNElement);
							bpmnElementListViewer.getList().setSelection(bpmnElementIndex);
							bpmnElementListViewer.refresh(true);
						}
					}
				} else {
					// If temporary file has not been updated but the user has modified the selection in the bpmnElementList
					if (yaoqiangSelection != null) {
						int bpmnElementIndex = bpmnElementList.indexOf(yaoqiangSelection);
						bpmnElementListViewer.getList().setSelection(bpmnElementIndex);
						bpmnElementListViewer.refresh(true);
					} else {
						// If there was no former selection an the temporary file was not updated
						MessageDialog.openWarning(shell, "Information", "There was no selection made in the Yaoqiang editor since the BPMN file has been loaded");
					}
				}
			}
		}
	}
	
	/**
	 * Helping method to find a equal BPMNElement in the list.
	 * @param bpmnElementToFind The element to which the element in the list has to be equal to
	 * @return The equal element from the list
	 */
	private BPMNElement findEqualInBPMNElementList(BPMNElement bpmnElementToFind) {
		for (Object bpmnObject : bpmnElementList) {
			if (bpmnObject instanceof BPMNElement) {
				BPMNElement bpmnElement = (BPMNElement) bpmnObject;
				if (bpmnElement.getID().equalsIgnoreCase(bpmnElementToFind.getID())) {
					return bpmnElement;
				}
			}
		}
		return null;
	}
}
