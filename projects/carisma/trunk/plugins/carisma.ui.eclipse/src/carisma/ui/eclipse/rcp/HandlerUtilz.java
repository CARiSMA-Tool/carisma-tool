package carisma.ui.eclipse.rcp;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.part.FileEditorInput;

import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

/** Utils class for the navigator view.
 * @author berghoff
 *
 */
public final class HandlerUtilz {
	
	/** hide default constructor.
	 * 
	 */
	private HandlerUtilz() {
		
	}
	
	
	
	
	
	
	/**
	 * @return the selection file in the navigator.
	 */
	public static IFile getSelectedNavigatorFile() {
		try {
			IWorkbenchWindow ww = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			ISelection selection = ww.getSelectionService().getSelection();
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			return (IFile) structuredSelection.getFirstElement();
		} catch (NullPointerException npe) {
			Logger.log(LogLevel.INFO, "No resource selected");
		}	
		return null;
	}
	
	/** 
	 * @return the file in opened in the activated editor.
	 */
	public static IFile getSelectedEditorFile() { 
		IWorkbenchPage page = org.eclipse.ui.PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	    try {
	    	FileEditorInput editorInput = (FileEditorInput) page.getActiveEditor().getEditorInput();
	    	return editorInput.getFile();
	    } catch (NullPointerException npe) {
	    	// do nothing
	    	Logger.log(LogLevel.INFO, "No resource selected");
	    }    	
		return null;
	}
}
