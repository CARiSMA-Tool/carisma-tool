package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.swt.widgets.Shell;

import carisma.xutils.regulatory.ui.first.AboutPopupMenu;


// TODO: Auto-generated Javadoc
/**
 * About-handler class for the About-command in the menu.
 *
 * @author bm
 */
public class AboutHandler extends AbstractHandler {

    /* (non-Javadoc)
     * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
     */
    @Override
    public Object execute(ExecutionEvent event) throws ExecutionException {
        // HandlerUtil.getActiveWorkbenchWindow(event).close();
        // commandID = org.eclipse.ui.help.aboutAction

        // creates new About-dialog
        new AboutPopupMenu(new Shell());

        return null;
    }

}