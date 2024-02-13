package carisma.evolution.uml2.umlchange.ui;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionDelegate;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.Carisma;
import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.analysis.CheckReference;
import carisma.core.checks.CheckRegistry;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
import carisma.evolution.DeltaFactoryCheck;
import carisma.evolution.EvolutionUtility;
import carisma.evolution.uml2.umlchange.UMLchangeParserCheck;

/**
 * 
 */
public class ConvertToUMLChangeAnalysis implements IObjectActionDelegate {
	/**
	 * the selected file to be converted.
	 */
	private IFile selectedFile = null;
	/**
	 * the Shell.
	 */
	private Shell shell;
	
	/**
	 * Constructor for Action1.
	 */
	public ConvertToUMLChangeAnalysis() {
		super();
	}
	
	/**
	 * Constant value for "- found check '".
	 */
	private static final String FOUND_CHECK = "- found check '";
	
	/**
	 * Constant value for "- did not found check '".
	 */
	private static final String DID_NOT_FOUND_CHECK = "- did not found check '";
	
	/**
	 * Check-ID of the check which has to move to the first position.
	 */
	private final String firstString = UMLchangeParserCheck.CHECK_ID;
	
	/**
	 * Check-ID of the check which has to move to the second position.
	 */
	private final String secondString = DeltaFactoryCheck.CHECK_ID;
	
	/**
	 * Check-ID of the check which has to move to the third position.
	 */
	private final String thirdString = "carisma.evolution.uml2.UMLModifierCheck";
			
	/**
	 * Check-ID of the check which has to move to the last position.
	 */
	private final String lastString = "carisma.evolution.io.ModelExporterCheck";

	/**
	 * @see IObjectActionDelegate#setActivePart(IAction, IWorkbenchPart)
	 * @param action The action
	 * @param targetPart The IWorkbenchPart
	 */
	@Override
	public final void setActivePart(final IAction action, final IWorkbenchPart targetPart) {
		this.shell = targetPart.getSite().getShell();
	}

	/**
	 * @see IActionDelegate#run(IAction)
	 * @param action The action
	 */
	@Override
	@SuppressWarnings("deprecation")
	public final void run(final IAction action) {
		// Init all variables
		CheckReference firstCheck = null;
		CheckReference secondCheck = null;
		CheckReference thirdCheck = null;
		CheckReference lastCheck = null;
		StringBuffer actionsPerformedString = new StringBuffer(60);
		boolean adfWasOpen = false;
		
		// Get the active Workbenchpage
		IWorkbenchPage activePage = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
				
		// Close the adf-File in the Eclipse Workbench if it is open
		for (IEditorPart iep : activePage.getEditors()) {
			if (iep.getEditorInput() instanceof FileEditorInput) {
				FileEditorInput fei = (FileEditorInput) iep.getEditorInput();
				if (fei.getFile().equals(this.selectedFile)) {
					activePage.closeEditor(iep, false);
					if (iep.isDirty()) {
						MessageDialog mDialog = new MessageDialog(
								this.shell, "Save and Launch", null, "Do you want to save the changes?", 0, new String[]{"OK", "Cancel"}, 0);
						mDialog.open();
						if (mDialog.getReturnCode() == 0) {
							iep.doSave(null);
						}
					}
					adfWasOpen = true;
				}
			}
		}
		
		// Read the adf-File
		Analysis analysisFile = AnalysisUtil.readAnalysis(this.selectedFile.getLocation().toOSString());
		
		// Find the corresponding checks
		List<CheckReference> checkList = analysisFile.getChecks();
		for (CheckReference checkref : checkList) {
			if (checkref.getCheckID().equals(this.firstString)) {
				firstCheck = checkref;
			}
			if (checkref.getCheckID().equals(this.secondString)) {
				secondCheck = checkref;
			}
			if (checkref.getCheckID().equals(this.thirdString)) {
				thirdCheck = checkref;
			}
			if (checkref.getCheckID().equals(this.lastString)) {
				lastCheck = checkref;
			}
		}
		
		// Move Checks to right position, or add them there if they miss
		if (firstCheck != null) {
			checkList.remove(firstCheck);
			actionsPerformedString.append(FOUND_CHECK);
			actionsPerformedString.append(this.firstString);
			actionsPerformedString.append("', moved to first position\n");
		} else {
			Carisma.getInstance().getCheckRegistry();
			firstCheck = CheckRegistry.createReference(Carisma.getInstance().getCheckRegistry().getCheckDescriptor(this.firstString));
			actionsPerformedString.append(DID_NOT_FOUND_CHECK);
			actionsPerformedString.append(this.firstString);
			actionsPerformedString.append("', added to first position\n");
		}
		checkList.add(0, firstCheck);
		
		if (secondCheck != null) {
			checkList.remove(secondCheck);
			actionsPerformedString.append(FOUND_CHECK);
			actionsPerformedString.append(this.secondString);
			actionsPerformedString.append("', moved to second position\n");
		} else {
			Carisma.getInstance().getCheckRegistry();
			secondCheck = CheckRegistry.createReference(Carisma.getInstance().getCheckRegistry().getCheckDescriptor(this.secondString));
			actionsPerformedString.append(DID_NOT_FOUND_CHECK);
			actionsPerformedString.append(this.secondString);
			actionsPerformedString.append("', added to second position\n");
		}
		checkList.add(1, secondCheck);
		
		if (thirdCheck != null) {
			checkList.remove(thirdCheck);
			actionsPerformedString.append(FOUND_CHECK);
			actionsPerformedString.append(this.thirdString);
			actionsPerformedString.append("', moved to third position\n");
		} else {
			Carisma.getInstance().getCheckRegistry();
			thirdCheck = CheckRegistry.createReference(Carisma.getInstance().getCheckRegistry().getCheckDescriptor(this.thirdString));
			actionsPerformedString.append(DID_NOT_FOUND_CHECK);
			actionsPerformedString.append(this.thirdString);
			actionsPerformedString.append("', added to third position\n");
		}
		checkList.add(2, thirdCheck);
		
		if (lastCheck != null) {
			checkList.remove(lastCheck);
			actionsPerformedString.append(FOUND_CHECK);
			actionsPerformedString.append(this.lastString);
			actionsPerformedString.append("', moved to last position\n");
		} else {
			Carisma.getInstance().getCheckRegistry();
			lastCheck = CheckRegistry.createReference(Carisma.getInstance().getCheckRegistry().getCheckDescriptor(this.lastString));
			actionsPerformedString.append(DID_NOT_FOUND_CHECK);
			actionsPerformedString.append(this.lastString);
			actionsPerformedString.append("', added to last position\n");
		}
		checkList.add(lastCheck);
//		KR: ab hier Aenderungen um aus non evo checks evo checks zu machen
		for (int i = 3; i < checkList.size() - 1; i++) { //ersten drei sind nicht zu behanlden, letzte auch nicht
			String checkID = checkList.get(i).getCheckID();

				String evoID = EvolutionUtility.getEvolutionCheck(checkID);
				if (evoID != null) {
					Carisma.getInstance().getCheckRegistry();
					CheckReference checkRef = CheckRegistry.createReference(
							Carisma.getInstance().getCheckRegistry().getCheckDescriptor(evoID));
					if (checkRef != null) {
						checkList.remove(i);
						checkList.add(i, checkRef);
						actionsPerformedString.append("- exchanged EvolutionCheck \"");
						actionsPerformedString.append(evoID);
						actionsPerformedString.append("\" for NonEvolutionCheck \"");
						actionsPerformedString.append(checkID);
						actionsPerformedString.append("\"\n");
					} else {
						actionsPerformedString.append("- no evolution check found for evolution check id");
						actionsPerformedString.append(evoID);
						actionsPerformedString.append("\"\n");
						//TODO gute Loesung?
					}
				} else {
					actionsPerformedString.append("- found no evolution check id for CARiSMA check id ");
					actionsPerformedString.append(checkID);
					actionsPerformedString.append("\"\n");
					//TODO gute Loesung?
				}
		}
		
		// Save to adf-File
		AnalysisUtil.storeAnalysis(analysisFile, this.selectedFile.getLocation().toOSString());
		
		// Reopen the adf-File if it was open before
		if (adfWasOpen) {
			try {
				IEditorInput newEditor = new FileEditorInput(this.selectedFile);
				activePage.openEditor(newEditor, "carisma.core.editors.AdfEditor");
			} catch (Exception e) {
				Logger.log(LogLevel.ERROR, e.getMessage(), e);
			}
		}
		// refresh resource
		if (this.selectedFile != null) {
			try {
				this.selectedFile.refreshLocal(IResource.DEPTH_ZERO, null);
			} catch (CoreException e) {
				Logger.log(LogLevel.INFO, "Could not refresh resource");
			}
		}
		
		// Success Message
		MessageDialog.openInformation(
				this.shell,
				"CARiSMA",
				"Successfully converted " + this.selectedFile.getName() + " to an UMLchange analysis\n" 
				+ "Actions performed:\n" 
				+ actionsPerformedString
				);
	}
	
	/**
	 * @see IActionDelegate#selectionChanged(IAction, ISelection)
	 * @param action The IAction
	 * @param selection The ISelection
	 */
	@Override
	public final void selectionChanged(final IAction action, final ISelection selection) {
		if (selection instanceof IStructuredSelection) {
            IStructuredSelection structuredSelection = (IStructuredSelection) selection;
            this.selectedFile = (IFile) structuredSelection.getFirstElement();
        }
	}


	/**
	 * Selection in AnalysisEditor part, initialization by Handler.
	 * @param window
	 * @return initialization was successful
	 */
	public final boolean initSelectionByEditor() {
		
		IWorkbenchPage page = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	    try {
	    	FileEditorInput editorInput = (FileEditorInput) page.getActiveEditor().getEditorInput();
	    	IFile file = editorInput.getFile();
	    	if (file != null && file.getName().endsWith(".adf")) {
	    		this.selectedFile = file;
	    		return true;
	    	}
	    } catch (NullPointerException npe) {
	    	// nothing selected, do nothing
	    	Logger.log(LogLevel.INFO, "No resource selected");
	    }
	   return false;
	}
	
	/**
	 * Selection in Project navigator, not in editor. 
	 * @return true, if an resource was selected
	 */
	public final boolean initSelectionByService() {
		
		try {
			IWorkbenchWindow ww = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			ISelection selection = ww.getSelectionService().getSelection();
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			this.selectedFile = (IFile) structuredSelection.getFirstElement();
			
			return true;
			
		} catch (NullPointerException npe) {
			Logger.log(LogLevel.INFO, "No resource selected");
		}
		return false;
	}
}
