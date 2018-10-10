/*******************************************************************************
 * Copyright (c) 2012 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.xutils.regulatory.importer.superior.ui;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.core.resources.ResourcesPlugin;

import carisma.xutils.regulatory.importer.superior.ui.log.LogViewEntrySet;

/**
 * The SettingsView for the superior importer GUI.
 * @author jkowald
 *
 */
public class SettingsView extends ViewPart {
	
	/**
	 * The ID string.
	 */
	public static final String ID = "carisma.xutils.regulatory.importer.superior.ui.SettingsView";
	
	/**
	 * The display object from the parent composite.
	 */
	private Display display;

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 * @param parent The parent composite
	 */
	@Override
	public final void createPartControl(final Composite parent) {
		this.display = parent.getDisplay();
		FormToolkit toolkit = new FormToolkit(this.display);
		
		// The basic GridLayout & -Data
		GridLayout layoutParent = new GridLayout(3, false);
		layoutParent.horizontalSpacing = 5;
		layoutParent.verticalSpacing = 5;
		parent.setLayout(layoutParent);
		GridData dataParent = new GridData();
		dataParent.verticalAlignment = SWT.FILL;
		parent.setLayoutData(dataParent);
		
		// The labels have to cover three columns of the grid
		final GridData labelData = new GridData(GridData.FILL_HORIZONTAL);
		labelData.horizontalSpan = 3;
		
		// The texts have to cover two columns of the grid
		final GridData textData = new GridData(GridData.FILL_HORIZONTAL);
		textData.horizontalSpan = 2;
		
		// The input "Choose Folder" section
		Label labelFolderInput = new Label(parent, SWT.LEFT); 
		labelFolderInput.setText("Choose the input folder");
		labelFolderInput.setLayoutData(labelData);
		
		Text textFolderInput = toolkit.createText(parent, "D:\\Workspaces\\TopcasedWorkspace\\SecureClouds-Zwischentreffen\\resources");				  
//		Text textFolderInput = toolkit.createText(parent, "");
		textFolderInput.setLayoutData(textData);
		textFolderInput.setMessage("The input folder should contain the folders 'bsisource', 'dtds', 'Gesetze' and 'MaRisk'");
			
		createBrowseInputButton(toolkit, parent, textFolderInput);
		
		// The output "Choose Folder" section
		Label labelFolderOutput = new Label(parent, SWT.LEFT); 
		labelFolderOutput.setText("Choose the output folder");
		labelFolderOutput.setLayoutData(labelData);
		
		Text textFolderOutput = toolkit.createText(parent, "D:\\Workspaces\\TopcasedWorkspace\\SecureClouds-Zwischentreffen\\resources");				  
//		Text textFolderOutput = toolkit.createText(parent, "");
		textFolderOutput.setLayoutData(textData);
		textFolderOutput.setMessage("The output folder will contain the ontology file after a successful execution");
		
		createBrowseOutputButton(toolkit, parent, textFolderOutput);
			
		// The output-file name section
		Label labelNameOutput = new Label(parent, SWT.LEFT); 
		labelNameOutput.setText("Choose the name of the output file (without '.owl')");
		labelNameOutput.setLayoutData(labelData);
		
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss");
		Date date = new Date();
		String textNameOutputDefault = "ontology_" + dateFormat.format(date);
//		Text textNameOutput = toolkit.createText(parent, textNameOutputDefault);
		Text textNameOutput = toolkit.createText(parent, "Ontology_demo");
		textNameOutput.setEditable(true);
		textNameOutput.setLayoutData(textData);
		
		Label labelDotOwl = new Label(parent, SWT.LEFT); 
		labelDotOwl.setText(".owl");
		
		// The output verbosity section
		Label labelVerbosity = new Label(parent, SWT.LEFT); 
		labelVerbosity.setText("Choose log output verbosity level");
		labelVerbosity.setLayoutData(labelData);		
		final Scale scaleVerbosity = createVerbosityScale(parent);
		
		// Empty label to move the import button into the next line
		Label labelSpacer = new Label(parent, SWT.LEFT); 
		labelSpacer.setText("");
		labelSpacer.setLayoutData(labelData);
		
		// Import Button
		createImportButton(toolkit, parent, 
				textFolderInput, textFolderOutput, textNameOutput, scaleVerbosity);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	@Override
	public void setFocus() {
		
	}
	
	/**
	 * Creating the Browse for Juris-Data button.
	 * @param toolkit An instance of a FormToolkit
	 * @param parent The parent composite
	 * @param folderText The swt text element which holds the path to the input folder
	 */
	private void createBrowseInputButton(final FormToolkit toolkit, final Composite parent, final Text folderText) {
		Button browseJuris = toolkit.createButton(parent, "Browse", SWT.PUSH);
		browseJuris.setToolTipText("Browse to the input folder");
		
		browseJuris.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(parent.getShell(), SWT.NULL);
				Table fileTable = new Table(dialog.getParent(), SWT.MULTI);
				dialog.setMessage("Browse to the input folder which should contain the folders 'bsisource', 'dtds', 'Gesetze' and 'MaRisk'");
				dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + System.getProperty("file.separator"));
				String path = dialog.open();
				if (path != null) {
					File file = new File(path);
					if (file.isFile()) {
						displayFiles(new String[] { file.toString()}, fileTable);
					} else {
						displayFiles(file.list(), fileTable);
					}
					folderText.setText(path);
		        }
			}
		});
	}
	
	/**
	 * Creating the Browse for Bsi-Data button.
	 */
	/*
	private void createBrowseBsiButton(final FormToolkit toolkit, final Composite parent, final Text folderText) {
		Button browseBsi = toolkit.createButton(parent, "Browse", SWT.PUSH);
		browseBsi.setToolTipText("Browse to the folder where the Juris- and MARisk Data lives");
		
		browseBsi.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(parent.getShell(), SWT.NULL);
				Table fileTable = new Table(dialog.getParent(), SWT.MULTI);
				dialog.setMessage("Browse to the folder where the BSI-Data lives");
				dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + System.getProperty("file.separator"));
				String path = dialog.open();
				if (path != null) {
					File file = new File(path);
					if (file.isFile()) {
						displayFiles(new String[] { file.toString()}, fileTable);
					} else {
						displayFiles(file.list(), fileTable);
					}
					folderText.setText(path);
		        }
			}
		});
	}*/
	
	/**
	 * Creating the Browse for output folder button.
	 * @param toolkit An instance of a FormToolkit
	 * @param parent The parent composite
	 * @param folderText The swt text element which holds the path to the output folder
	 */
	private void createBrowseOutputButton(final FormToolkit toolkit, final Composite parent, final Text folderText) {
		Button browseBsi = toolkit.createButton(parent, "Browse", SWT.PUSH);
		browseBsi.setToolTipText("Browse to the folder where the output files should be placed");
		
		browseBsi.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				DirectoryDialog dialog = new DirectoryDialog(parent.getShell(), SWT.NULL);
				Table fileTable = new Table(dialog.getParent(), SWT.MULTI);
				dialog.setMessage("Browse to the folder where the output files should be placed");
				dialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + System.getProperty("file.separator"));
				String path = dialog.open();
				if (path != null) {
					File file = new File(path);
					if (file.isFile()) {
						displayFiles(new String[] { file.toString()}, fileTable);
					} else {
						displayFiles(file.list(), fileTable);
					}
					folderText.setText(path);
		        }
			}
		});
	}
	
	/**
	 * Creating the log verbosity level scale with its value output label.
	 * @param parent The parent composite
	 * @return The created Scale object
	 */
	private Scale createVerbosityScale(final Composite parent) {
		final String[] scaleVerbosityLevel = 
			{"Very low", "Low", "Middle", "High", "Very high"};
		final Scale scaleVerbosity = new Scale(parent, SWT.LEFT);
		scaleVerbosity.setMinimum(1);
		scaleVerbosity.setMaximum(10);
		scaleVerbosity.setIncrement(1);
		scaleVerbosity.setPageIncrement(1);
		scaleVerbosity.setSelection(10);
		scaleVerbosity.setToolTipText("1 = low, 10 = high");
		
		final Label labelVerbosityLevel = new Label(parent, SWT.LEFT);
		labelVerbosityLevel.setText("Level: Very high");
		
		scaleVerbosity.addListener(SWT.Selection, new Listener() {
			public void handleEvent(final Event event) {
				int value = scaleVerbosity.getSelection();
				labelVerbosityLevel.setText("Level: " + scaleVerbosityLevel[(value - 1) / 2]);
			}
		});
		
		return scaleVerbosity;
	}
	
	/**
	 * Creating the Import button.
	 * @param toolkit An instance of a FormToolkit
	 * @param parent The parent composite
	 * @param textFolderInput The swt text element which holds the path to the input folder
	 * @param textFolderOutput The swt text element which holds the path to the output folder
	 * @param textNameOutput The swt text element which holds the output file name
	 * @param scaleVerbosity The swt scale element which holds the log verbosity level
	 */
	private void createImportButton(final FormToolkit toolkit, final Composite parent, 
			final Text textFolderInput, final Text textFolderOutput, 
			final Text textNameOutput, final Scale scaleVerbosity) {
		Button importButton = toolkit.createButton(parent, "Import", SWT.PUSH | SWT.BOTTOM);
		
		GridData importButtonData = new GridData();
		importButtonData.verticalAlignment = SWT.FILL;
		importButtonData.horizontalAlignment = SWT.FILL;
		
		importButton.setToolTipText("Start creating the ontology");
		
		importButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (checkSuperiorImporterParameters(parent.getShell(), textFolderInput.getText(), textFolderOutput.getText())) {
					String resourceBaseFolderPath = textFolderInput.getText()  + File.separator;
					String outputFileName = textNameOutput.getText();
					if (!outputFileName.endsWith(".owl")) {
						outputFileName += ".owl";
					}
					String ontologySavepath = textFolderOutput.getText() + File.separator + outputFileName;
					int logVerbosityLevel = scaleVerbosity.getSelection();
					callSuperiorImporter(resourceBaseFolderPath, ontologySavepath, logVerbosityLevel);	
				}
			}
		});
	}

	/**
	 * Managing the browse dialogs file tables.
	 * @param files The files to be displayed in the fileTable
	 * @param fileTable The fileTable where the files are displayed
	 */
	public final void displayFiles(final String[] files, final Table fileTable) {
	    // Removes all existing table items.
		fileTable.removeAll();
	
	    for (int i = 0; files != null && i < files.length; i++) {
	    	TableItem item = new TableItem(fileTable, SWT.NULL);
	    	item.setText(files[i]);
	    }
	}
	
	/**
	 * Creates the SuperiorImporter instance and calls the parser.
	 * @param resourceBaseFolderPath The path where all the folders containing the regulatory documents are
	 * @param ontologySavepath The path where the ontology file will be saved
	 * @param logVerbosityLevel The verbosity level of the log entries
	 */
	private void callSuperiorImporter(final String resourceBaseFolderPath, final String ontologySavepath, final int logVerbosityLevel) {
		LogView logView = (LogView) SuperiorImporterGui.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().findView(LogView.ID);
		LogViewEntrySet logViewEntrySet = logView.getLogInput();
		logViewEntrySet.setLogVerbosityLevel(logVerbosityLevel);
			
		// Create a SuperiorImporterThread
		SuperiorImporterThread importerThread = new SuperiorImporterThread(logViewEntrySet, resourceBaseFolderPath, ontologySavepath);
		importerThread.setName("SuperiorImporterThread");
			
		// Delete the following try-catch block if the focus should not change to the log view
		try {
			SuperiorImporterGui.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().showView(LogView.ID);
		} catch (PartInitException pie) {
			pie.printStackTrace();
		}
			
		// Start importing/parsing Thread
		importerThread.start();
	}
	
	/**
	 * Validates the paths to the input and the output folder of the superior importer.
	 * @param shell The Shell object, where the warning dialog is displayed
	 * @param pathInput The path to the input folder as a string
	 * @param pathOutput The path to the output folder as a string
	 * @return A boolean which indicates the occurrence of warnings
	 */
	private boolean checkSuperiorImporterParameters(final Shell shell, 
			final String pathInput, 
			final String pathOutput) {
		boolean result = true;
		String information = "";
		
		// Check the input path
		if (pathInput == "") {
			information += "Warning: Path to the input data folder is not specified!" + System.getProperty("line.separator");
			result = false;
		} else {
			File pathInputDir = new File(pathInput);
			if (!pathInputDir.isDirectory()) {
				information += "Warning: Specified path to the input data folder is not a directory!" + System.getProperty("line.separator");
			}
		}
		if (pathOutput == "") {
			information += "Warning: Path to the output folder is not specified!" + System.getProperty("line.separator");
			result = false;
		} else {
			File pathOutputDir = new File(pathOutput);
			if (!pathOutputDir.isDirectory()) {
				information += "Warning: Specified path to the output folder is not a directory!" + System.getProperty("line.separator");
			}
		}
		if (!result) {
			MessageDialog.openWarning(shell, "Warning", information);
		}
		return result;
	}
		
	/**
	 * The content provider class is responsible for providing objects to the
	 * view. It can wrap existing objects in adapters or simply return objects
	 * as-is. These objects may be sensitive to the current input of the view,
	 * or ignore it and always show the same content (like Task List, for
	 * example).
	 */
	class ViewContentProvider implements IStructuredContentProvider {
		@Override
		public void inputChanged(final Viewer v, final Object oldInput, final Object newInput) {
		}

		@Override
		public void dispose() {
		}

		@Override
		public Object[] getElements(final Object parent) {
			if (parent instanceof Object[]) {
				return (Object[]) parent;
			}
	        return new Object[0];
		}
	}

	/**
	 * The views label provider.
	 */
	class ViewLabelProvider extends LabelProvider implements
			ITableLabelProvider {
		@Override
		public String getColumnText(final Object obj, final int index) {
			return getText(obj);
		}

		@Override
		public Image getColumnImage(final Object obj, final int index) {
			return getImage(obj);
		}

		@Override
		public Image getImage(final Object obj) {
			return PlatformUI.getWorkbench().getSharedImages().getImage(
					ISharedImages.IMG_OBJ_ELEMENT);
		}
	}
}