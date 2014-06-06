package carisma.ui.eclipse.editors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

import carisma.ui.eclipse.CarismaGUI;

/**
 * Class for creating the new ADF File. This class overwrites the WizardNewFileCreationPage 
 * and is similar to it, but this page also converts the returned filename into an IFile 
 * @see WizardNewFileCreationPage
 */
public class AdfModelWizardNewFileCreationPage extends WizardNewFileCreationPage {

	/**
	 * @param pageName name of the wizard page
	 * @param selection selection
	 */
	public AdfModelWizardNewFileCreationPage(final String pageName,
			final IStructuredSelection selection) {
		super(pageName, selection);
		setDescription("Create a new analysis file (.adf)");
	}

	/**
	 * @param parent the Composite
	 */
	@Override
	public final void createControl(final Composite parent) {
		super.createControl(parent);
		PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), CarismaGUI.PLUGIN_ID + "." + AdfModelWizard.ADFWIZARDCONTEXTID);
		setPageComplete(validatePage());
	}
	
	@Override
	protected final void createAdvancedControls(final Composite parent) {
		Composite emptyComposite = new Composite(parent, SWT.NONE);
		super.createAdvancedControls(emptyComposite);
		emptyComposite.setVisible(false);
	}
			
	/**
	 * @return the IFile of the choosen model filename
	 */
	public final IFile getTargetFile() {
		return ResourcesPlugin.getWorkspace().getRoot()
				.getFile(getContainerFullPath().append(getFileName()));
	}
	
	@Override
	protected final boolean validatePage() {
		if (!super.validatePage()) {
			return false;
		}
		
		String filename = super.getFileName();
		
		// file name contains a dot
		if (filename.contains(".")) {
			setErrorMessage("Filename contains invalid dot character");
			return false;
		}
		// file name contains two whitespaces in a row
		if (filename.contains("  ")) {
			setErrorMessage("Filename contains two whitespaces in a row");
			return false;
		}
		// filename starts with a whitespace
		if (!filename.equals(filename.trim())) {
			setErrorMessage("Filename begins (a) with whitespace(s)");
			return false;
		}
		
		setErrorMessage(null);
		return true;
	}
	
	@Override
	public final void performHelp() {
		PlatformUI.getWorkbench().getHelpSystem().displayHelp(CarismaGUI.PLUGIN_ID + "." + AdfModelWizard.ADFWIZARDCONTEXTID);
	}
	
	/**
	 * Called by the previous page to set the suggested folder and filename for the adf file.
	 * @param name Name of the model file selected for the analysis
	 * @param path Path to the model file selected for the analysis
	 */
	protected final void suggestFileNameAndFolder(final String name, final IPath path) {
		// Suggest Filename
		String fileExt = ".";			
		String sourceFilenameWithoutExt = name.substring(0, name.indexOf(fileExt));
		String filename = sourceFilenameWithoutExt + "_analysis";
		super.setFileName(filename);
		super.setContainerFullPath(path);
	}
}
