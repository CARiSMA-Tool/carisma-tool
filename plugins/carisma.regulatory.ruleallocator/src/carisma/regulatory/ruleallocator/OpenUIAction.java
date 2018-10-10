package carisma.regulatory.ruleallocator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.WorkbenchException;

/**
 * An Action which switches the perspective to the packages' perspective.
 * @author jkowald
 */
public class OpenUIAction extends AbstractHandler implements IHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			String id = Perspective.ID;
			try {
				PlatformUI.getWorkbench().showPerspective(id, window);
			} catch (WorkbenchException ex) {
				System.out.println("Cannot open the packages' perspective!");
			}
		}
		return null;
	}
}
