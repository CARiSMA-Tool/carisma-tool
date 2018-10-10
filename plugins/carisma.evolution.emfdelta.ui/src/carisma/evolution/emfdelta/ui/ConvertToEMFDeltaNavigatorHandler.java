package carisma.evolution.emfdelta.ui;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;

/**
 * 
 * @author Johannes Kowald
 *
 */
public class ConvertToEMFDeltaNavigatorHandler extends AbstractHandler implements IHandler {
	/**
	* 
	*/
	private ConvertToEMFDeltaAnalysis convertAction = null;

	/**
	* Constructor.
	*/
	public ConvertToEMFDeltaNavigatorHandler() {
		convertAction = new ConvertToEMFDeltaAnalysis();
	}


	@Override
	public final Object execute(final ExecutionEvent event) throws ExecutionException {

		if (convertAction.initSelectionByService()) {
			convertAction.run(null);
		}
		return null;
	}
}
