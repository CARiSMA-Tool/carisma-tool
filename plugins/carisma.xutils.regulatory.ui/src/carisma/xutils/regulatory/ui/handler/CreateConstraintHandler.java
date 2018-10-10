package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import carisma.xutils.regulatory.ui.first.RegulationsView;
import carisma.xutils.regulatory.ui.model.Constants;


// TODO: Auto-generated Javadoc
/**
 * Search-handler class for the Search-command in the menu.
 *
 * @author bm
 */
public class CreateConstraintHandler extends AbstractHandler {

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        // calls the search-method
        RegulationsView.createNewConstraint();
        return null;
    }

}