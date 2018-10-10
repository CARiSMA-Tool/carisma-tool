package carisma.ui.eclipse.rcp;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;

/**
 * sdv.
 * @author jkowald
 *
 */
public class OpenCarismaPerspectiveHandler extends AbstractHandler {

	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			String id = carisma.ui.eclipse.Perspective.ID;
			try {
				PlatformUI.getWorkbench().showPerspective(id, window);
			} catch (WorkbenchException ex) {
				System.out.println("Cannot open CARiSMA perspective!");
			}
		}
		return null;
	}

}
