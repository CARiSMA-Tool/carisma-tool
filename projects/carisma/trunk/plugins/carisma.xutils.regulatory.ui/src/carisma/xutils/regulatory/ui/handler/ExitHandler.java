package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

// TODO: Auto-generated Javadoc
/**
 * Exit-handler class for the Exit-command in the menu.
 *
 * @author bm
 */
public class ExitHandler extends AbstractHandler {

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
    	// changes the perspective
//    	RegulatoryUISourceProvider.getInstance().perspectiveChanged(true);
        // closes the active window
        HandlerUtil.getActiveWorkbenchWindow(event).close();
        return null;
    }

}