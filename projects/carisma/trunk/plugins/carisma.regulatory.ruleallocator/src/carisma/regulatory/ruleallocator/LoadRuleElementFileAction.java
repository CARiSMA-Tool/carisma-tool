package carisma.regulatory.ruleallocator;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * An Action related to the 'Load RuleElement File' menu entry which loads a RuleElement file for the GUI.
 * @author jkowald
 *
 */
public class LoadRuleElementFileAction extends AbstractHandler implements IHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window != null) {
			ViewPart viewPart = (ViewPart) window.getActivePage().findView("carisma.regulatory.ruleallocator.ruleallocatorview");
			if (viewPart instanceof RuleAllocatorView) {
				RuleAllocatorView ruleAllocatorView = (RuleAllocatorView) viewPart;
				Shell shell = ruleAllocatorView.getShell();
	        	FileDialog fileDialog = new FileDialog(shell);
	        	fileDialog.setFilterExtensions(new String[]{"*.xmi"});
	        	String filepath = fileDialog.open();
	        	if (filepath != null && filepath != "") {
	        		ruleAllocatorView.loadRuleElementFile(filepath);
	        	}
			}
		}
		return null;
	}
}