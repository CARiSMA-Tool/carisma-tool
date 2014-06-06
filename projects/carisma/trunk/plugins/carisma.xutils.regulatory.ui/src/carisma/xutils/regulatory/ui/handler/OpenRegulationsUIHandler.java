package carisma.xutils.regulatory.ui.handler;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;

import carisma.xutils.regulatory.ui.first.Perspective;

// TODO: Auto-generated Javadoc
/**
 * The Class OpenRegulationsUIHandler.
 */
public class OpenRegulationsUIHandler extends AbstractHandler implements IHandler{

	/* (non-Javadoc)
	 * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.ExecutionEvent)
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		
		if (window != null) {
			try {
				window.getWorkbench().showPerspective(Perspective.ID, window);
//				RegulatoryUISourceProvider.getInstance().perspectiveChanged(true);
			} catch (Exception e) {
				System.err.println("Cannot open the regulations ui.");
			}
		}
		return null;
	}

}
