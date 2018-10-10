package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import carisma.xutils.regulatory.ui.controller.MainController;
import carisma.xutils.regulatory.ui.first.ConfigurationView;
import carisma.xutils.regulatory.ui.first.GeneralRelationPopupMenu;
import carisma.xutils.regulatory.ui.model.Constants;


// TODO: Auto-generated Javadoc
/**
 * GeneralRelation-handler class for the GeneralRelation-command in the menu.
 *
 * @author bm
 */
public class GeneralRelationHandler extends AbstractHandler {

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {

        if (ConfigurationView.getLabelList() == null) {
        	IWorkbenchPage activePage = null;
            // if no RuleElements are created, the Configuration-tab
            // must be called to initialize variables and create tree
            try {
                activePage = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow().getActivePage();
                String viewIdToOpen = Constants.CLS_CONFIGURATION;
                activePage.showView(viewIdToOpen);
            } catch (PartInitException p) {
                p.printStackTrace();
            }
            MainController.getInstance().makeMessageBox(new Shell(),
            		"No RuleElements defined. Please define new RuleElements" +
            		" or load an Ontology.");
        } else {

            // creates new general Relation-dialog
            new GeneralRelationPopupMenu(new Shell());
        }
        return null;
    }

}
