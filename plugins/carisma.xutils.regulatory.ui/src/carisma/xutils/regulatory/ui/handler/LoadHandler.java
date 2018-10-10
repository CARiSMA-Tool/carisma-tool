package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;

import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.first.ConfigurationView;
import carisma.xutils.regulatory.ui.first.RegulationsView;
import carisma.xutils.regulatory.ui.model.Constants;


// TODO: Auto-generated Javadoc
/**
 * Load-handler class for the Load-command in the menu.
 *
 * @author bm
 */
public class LoadHandler extends AbstractHandler {

    /** The ontology controller. */
    private MainController ontologyController;

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

        if (ConfigurationView.getLabelList() == null) {

            // if no RuleElements are created, the Configuration-tab
            // must be called to initialize variables and create tree
            try {
                IWorkbenchPage activePage = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                String viewIdToOpen = Constants.CLS_CONFIGURATION;
                activePage.showView(viewIdToOpen);
            } catch (PartInitException p) {
                p.printStackTrace();
            }

        }

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
        FileDialog dlg = new FileDialog(shell, SWT.MULTI);
        dlg.setFilterNames(FILTER_NAMES);
        dlg.setFilterExtensions(FILTER_EXTS);
        dlg.setText("Open ...");
        String fileName = dlg.open();

        if (fileName != null) {

            ontologyController = RegulationsView.getOntologyController();

            ontologyController.loadOntology(fileName);
            RegulationsView.preLoadRuleElement();

            System.out.println("File opened: " + fileName);

        } else {
            // closes the main window of the Programm (not the Loading window) DB
//            HandlerUtil.getActiveWorkbenchWindow(event).close();
        }
        return null;
    }

}