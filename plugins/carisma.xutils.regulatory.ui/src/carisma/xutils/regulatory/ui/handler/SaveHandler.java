package carisma.xutils.regulatory.ui.handler;

import java.io.File;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import carisma.regulatory.ontology.utils.RegulatoryOntologyHelper;
import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.first.RegulationsView;
import carisma.xutils.regulatory.ui.model.Constants;

// TODO: Auto-generated Javadoc
/**
 * Save-handler class for the Save-command in the menu.
 *
 * @author bm
 */
public class SaveHandler extends AbstractHandler {

    /** The ontology controller. */
    private MainController conroller;
    
    /** The roh. */
    private RegulatoryOntologyHelper roh;

    // filter names which are displayed in the file dialog
    /** The Constant FILTER_NAMES. */
    private static final String[] FILTER_NAMES = {
            "Protégé Ontology Files (*.owl)", "All Files (*.*)" };

    // filter extensions which are used to filter displayed files
    /** The Constant FILTER_EXTS. */
    private static final String[] FILTER_EXTS = { "*.owl", "*.*" };

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        try {
            // switches to the Regulations-tab
            IWorkbenchPage activePage = PlatformUI.getWorkbench()
                    .getActiveWorkbenchWindow().getActivePage();
            String viewIdToOpen = Constants.CLS_REGULATIONS;
            activePage.showView(viewIdToOpen);
        } catch (PartInitException p) {
            p.printStackTrace();
        }
	    Shell shell = new Shell();
	    FileDialog dlg = new FileDialog(shell, SWT.SAVE);
	    dlg.setFilterNames(FILTER_NAMES);
	    dlg.setFilterExtensions(FILTER_EXTS);
	    dlg.setFileName("Ontology.owl");
	    // dlg.setFilterPath("/home/bm/workspace/carisma.regulatory/resources/");
	    String fileName = dlg.open();
	    conroller = RegulationsView.getOntologyController();
	    roh = conroller.getROH();
	    if (fileName != null) {
	      	try {
		       	roh.saveOntologyToFile(new File(fileName));
		       	System.out.println("Datei erstellt: " + fileName);
	       	} catch (Exception e ) {
	        		conroller.makeMessageBox(shell, "Failed to store the File.");
	       	}
	    }        
        return null;
    }

}