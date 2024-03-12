package carisma.ui.eclipse.editors;

import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import carisma.core.models.ModelType;
import carisma.ui.eclipse.CarismaGUI;

import java.util.logging.Logger;


/**
 * Class ModelWizardDetailsPage.
 */
public class AdfModelWizardDetailsPage extends WizardPage {
	
	private static final Logger logger = Logger.getLogger(AdfModelWizardDetailsPage.class.getName());
	
	/**
	 * The current instance of this page.
	 */
	private static AdfModelWizardDetailsPage iNSTANCE;
	
	/**
	 * The model file as an IFile instance.
	 */
	private IFile sourceFile;
	
	/**
	 * The type of the model.
	 */
	private String modelType;
	
	/**
	 * Contains the model file extensions which are valid.
	 */
	String[] extensionFilter;
	
	/**
	 * The second page of the AdfModelWizard.
	 */
	private AdfModelWizardNewFileCreationPage newFilePage = null;
	
	/**
	 * The text box which displays the path to the model file.
	 */
	private Text pathToModelFileText = null;
	
	/**
	 * Constructor.
	 * @param pageName the name of the wizard page
	 * @param newFilePage The second page of the AdfModelWizard
	 */
	protected AdfModelWizardDetailsPage(final String pageName, final AdfModelWizardNewFileCreationPage newFilePage) {
		super(pageName);
		this.newFilePage = newFilePage;
		setDescription("Choose the model file and its type");
		iNSTANCE = this;
	}

	/**
	 * @param parent the Composite
	 */
	@Override
	public final void createControl(final Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		GridLayout gridLayout = new GridLayout(3, false);
		gridLayout.marginHeight = (parent.getStyle() == SWT.BORDER) ? 0 : 2;
		gridLayout.marginWidth  = (parent.getStyle() == SWT.BORDER) ? 0 : 2;
		composite.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		composite.setLayout(gridLayout);
		
		PlatformUI.getWorkbench().getHelpSystem().setHelp(
				composite,
				CarismaGUI.PLUGIN_ID + "." + AdfModelWizard.ADFWIZARDCONTEXTID);
		
		new Label(composite, SWT.NONE).setText("Select model type");

		final Combo typeCombo = new Combo(composite, SWT.BORDER | SWT.READ_ONLY);
		GridData gridDataSpan = new GridData(GridData.FILL_HORIZONTAL);
		gridDataSpan.horizontalSpan = 2;
		typeCombo.setLayoutData(gridDataSpan);
		final List<ModelType> modeltypeList = CarismaGUI.getModelTypeRegistry().getSupportedTypes();
		String[] types = new String[modeltypeList.size()];
		int umlIndex = 0;
		for (int i = 0; i < modeltypeList.size(); i++) {
			types[i] = modeltypeList.get(i).getName();
			if ("UML2".equalsIgnoreCase(types[i])) {
				umlIndex = i;
			}
		}
		typeCombo.setItems(types);
		typeCombo.select(umlIndex);
		this.extensionFilter = modeltypeList.get(typeCombo.getSelectionIndex()).getFileExtension().split(",");
		setModelType(typeCombo.getItem(typeCombo.getSelectionIndex()));
		typeCombo.setToolTipText("Select a model type from the given list");

		typeCombo.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				setModelType(typeCombo.getItem(typeCombo
						.getSelectionIndex()));
				AdfModelWizardDetailsPage.this.extensionFilter = modeltypeList.get(typeCombo.getSelectionIndex()).getFileExtension().split(",");
				setPageComplete(validatePage());
			}
		});
		
		new Label(composite, SWT.NONE).setText("Select the model file");

		this.pathToModelFileText = new Text(composite, SWT.SINGLE | SWT.BORDER);
		this.pathToModelFileText.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		this.pathToModelFileText.setText("");
		this.pathToModelFileText.setEditable(false);
		this.pathToModelFileText.setToolTipText("Displays the path to the selected model file");

		createBrowse(composite, this.pathToModelFileText);
		 
		setControl(composite);
	}
	
	/**
	 * @param sourceFile set the file that has to be analyzed, e.g. model.uml
	 */
	public final void setSourceFile(final IFile sourceFile) {
		this.sourceFile = sourceFile;
	}

	/**
	 * @return the source file for the analysis, e.g. "model.uml"
	 */
	public final IFile getSourceFile() {
		return this.sourceFile;
	}

	/**
	 * @param analyseType set the model type, e.g. "UML2"
	 */
	public final void setModelType(final String analyseType) {
		if (analyseType != null && !"".equals(analyseType)) {
			this.modelType = analyseType;
		}
	}

	/**
	 * @return the model type, e.g. "UML2"
	 */
	public final String getModelType() {
		return this.modelType;
	}
	
	/**
	 * Validates the input data of the Page.
	 * @return Success of wizard page validation
	 */
	protected final boolean validatePage() {
		if ((getSourceFile() != null) 
				&& (getModelType() != null)
				&& !"".equals(getModelType())) {
			if (Arrays.asList(this.extensionFilter).contains(getSourceFile().getFileExtension())) {
				this.setErrorMessage(null); 
				setPageComplete(true);
				return true;					
			}
			this.setErrorMessage("Model file is not of type " + getModelType());
			return false;					
		}
		setDescription("Choose the model file and its type");
		setPageComplete(false);
		return false;
	}

	@Override
	public final void performHelp() {
		PlatformUI.getWorkbench().getHelpSystem().displayHelp(CarismaGUI.PLUGIN_ID + "." + AdfModelWizard.ADFWIZARDCONTEXTID);
	}
	
	/**
	 * Creates the 'Browse Workspace' button and the corresponding dialog.
	 * @param composite The composite where the button is placed
	 * @param modelFileText The text field, which displays the model file name
	 */
	private void createBrowse(final Composite composite, final Text modelFileText) {
		Button browseWS = new Button(composite, SWT.PUSH);
		browseWS.setText("Browse");
		browseWS.setToolTipText("Choose the model file from your workspace");
		
		GridData gridDataSpan = new GridData(GridData.HORIZONTAL_ALIGN_END);
		browseWS.setLayoutData(gridDataSpan);

		browseWS.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				openFileDialog(composite, true);
			}
		});
	}
	
	/**
	 * Opens a FileDialog and sets its extension and path filter.
	 * @param composite The parent composite
	 * @param onlyWorkspace Indicates if the root path is the workspace location
	 */
	void openFileDialog(final Composite composite, final boolean onlyWorkspace) {
		FileDialog fileDialog = new FileDialog(composite.getShell(), SWT.OPEN);
		
		// Filter the files via the chosen extension
		final String[] fileDialogExtensionFilter = new String[this.extensionFilter.length];
		int iterate = 0;
		for (String singleExtension : this.extensionFilter) {
			fileDialogExtensionFilter[iterate++] = "*." + singleExtension;
		}
		fileDialog.setFilterExtensions(fileDialogExtensionFilter);
		
		// Set the root directory to workspace if indicated
		if (onlyWorkspace) {
			System.out.println("WORKSPACE PATH: " + ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString());
			fileDialog.setFilterPath(ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString());
		}
		
		if (getSourceFile() != null) {
			fileDialog.setFileName(getSourceFile().getFullPath().toOSString());
		}
		
		String filepath = fileDialog.open();
		if (filepath != null) {
			try {
				IFile modelIFile = getLinkedIFile(filepath);
				setSourceFile(modelIFile);
				this.pathToModelFileText.setText(getSourceFile().getFullPath().toString());
				this.newFilePage.suggestFileNameAndFolder(getSourceFile().getName(), getSourceFile().getFullPath());
				setPageComplete(validatePage());
			} catch (Exception exc) {
				iNSTANCE.setErrorMessage("Must select file");
				logger.warning("Error message: " + exc.getMessage());
			}
		}
	}
	
	/**
	 * Returns the IFile of the selected model file and creates, if necessary, a link to a non workspace file.
	 * @param filepath Path to the model file
	 * @return The IFile object of the selected file
	 * @throws CoreException If the IFile could not be created
	 */
	private static IFile getLinkedIFile(final String filepath) throws CoreException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IPath location = Path.fromOSString(filepath);
		IFile modelIFile = workspace.getRoot().getFileForLocation(location);
		
		if (modelIFile != null) {
			// If the file is in scope of the workspace
			return modelIFile;
		} 
		// If the file is outside the scope of the workspace
		IProject projectExternal = workspace.getRoot().getProject("External Files");
		if (!projectExternal.exists()) {
			projectExternal.create(null);
		}
		if (!projectExternal.isOpen()) {
			projectExternal.open(null);
		}
		modelIFile = projectExternal.getFile(location.lastSegment());
		if (!modelIFile.exists()) {
			modelIFile.createLink(location, IResource.NONE, null);
			projectExternal.refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
		}
		return modelIFile;
	}
}
