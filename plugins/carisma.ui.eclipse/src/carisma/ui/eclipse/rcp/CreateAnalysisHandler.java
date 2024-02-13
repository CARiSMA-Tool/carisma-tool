package carisma.ui.eclipse.rcp;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.AnalysisUtil;
import carisma.core.models.ModelType;
import carisma.core.util.Utils;
import carisma.ui.eclipse.CarismaGUI;

public class CreateAnalysisHandler extends AbstractHandler {

	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		String name = "";
		IFile selectedFile = HandlerUtilz.getSelectedNavigatorFile(); 
		if (selectedFile != null) {
			String ext = "." + selectedFile.getFileExtension();			
			name = selectedFile.getName().substring(0, selectedFile.getName().indexOf(ext));
			String file = selectedFile.getLocation().toString().substring(0, selectedFile.getLocation().toString().indexOf(ext));
			file = Utils.incrementFileNameIfNecessary(file + "_analysis.adf");
			if (initAdfFile(name + "Analyse", selectedFile, file)) {
//				MessageDialog.openInformation(
//						shell,
//						"CARiSMA",
//						"Created analysis for " + name + ":\n" + file);
				//TODO Fehler ausgabe!!
			}
		} else {
//			MessageDialog.openError(shell, "Carisma", "No file selected!");
			//TODO Fehler ausgabe!!
		}
		// refresh resources
		try {
			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			IProject[] projects = workspaceRoot.getProjects();
			for (IProject projectToRefresh : projects) {
				projectToRefresh.refreshLocal(IResource.DEPTH_INFINITE, null);
			}
		} catch (CoreException e) {
			//TODO Fehler ausgabe
//			System.out.println("could not refresh resource");
		}

		return null;
	}
	
	/**
	 * Writes the initiale plugin-list to file.
	 * @param analyseName The name of the analysis
	 * @param modelFile The corresponding modelFile
	 * @param analyseFile Path to the AnalyseFile
	 * @return Returns false if the modeltype is unknown, otherwise true is returned.
	 */
	public final static boolean initAdfFile(final String analyseName, final IFile modelFile, final String analyseFile) {
		ModelType type = CarismaGUI.getModelTypeRegistry().getTypeForExtension(modelFile.getFileExtension());
		if (type == null) {

			//TODO Fehler ausgabe!!
//			MessageDialog.openError(shell, "CARiSMA", "Unable to determine modeltype for file\n" + modelFile.getName());
			return false;
		}
		Analysis analysis = new Analysis(analyseName, type.getName(), modelFile);
		AnalysisUtil.storeAnalysis(analysis, analyseFile);
		return true;	
	}

}
