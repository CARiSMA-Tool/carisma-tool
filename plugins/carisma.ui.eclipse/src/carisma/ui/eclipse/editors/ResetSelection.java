package carisma.ui.eclipse.editors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;


/** Resets the saved editor selection of an analysis.
 * 
 * @author bberghoff
 *
 */
public class ResetSelection implements IObjectActionDelegate {

	/** 
	 * Selected analysis file.
	 */
	private IFile selectedFile;
	
	@Override
	public final void run(final IAction action) {
		Logger.log(LogLevel.DEBUG, "Test Message");

		Analysis analysis = AnalysisUtil.readAnalysis(this.selectedFile.getLocation()
				.toOSString());
		analysis.setSelectedEditorId("");
		
		// save analysis 	
		AnalysisUtil.storeAnalysis(analysis, this.selectedFile.getLocation().toOSString());
		// refresh resource

		IWorkbenchPage[] pages = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getPages();
		try {
			this.selectedFile.refreshLocal(IResource.DEPTH_ZERO, null);
		} catch (CoreException e) {
			Logger.log(LogLevel.INFO, "Could not refresh resource");
		}
		for (IWorkbenchPage page : pages) {
			for (IEditorReference editorRef : page.getEditorReferences()) {
				if (editorRef != null 
						&& this.selectedFile.getName().equals(editorRef.getName())
						&& editorRef.getEditor(false) instanceof AdfEditor) {
					((AdfEditor) editorRef.getEditor(false)).loadAnalysis();
				}
			}
		}
	}

	@Override
	public final void selectionChanged(final IAction action, final ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			this.selectedFile = (IFile) structuredSelection.getFirstElement();
		}
	}

	@Override
	public void setActivePart(final IAction action, final IWorkbenchPart targetPart) {
		// TODO Auto-generated method stub
		
	}

}
