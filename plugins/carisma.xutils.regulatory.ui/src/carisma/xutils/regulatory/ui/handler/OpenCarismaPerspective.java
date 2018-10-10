package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;



// TODO: Auto-generated Javadoc
/**
 * The Class OpenCarismaPerspective.
 */
public class OpenCarismaPerspective extends AbstractHandler implements IHandler {

	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			String id = "carisma.perspective";
			try {
				PlatformUI.getWorkbench().showPerspective(id, window);
			} catch (WorkbenchException ex) {
				System.out.println("Cannot open carisma perspective!");
			}
		}
		return null;
	}

}
