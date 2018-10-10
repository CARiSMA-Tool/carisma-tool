package carisma.regulatory.ruleallocator;

import org.eclipse.swt.graphics.Point;

import org.eclipse.ui.application.ActionBarAdvisor;
import org.eclipse.ui.application.IActionBarConfigurer;
import org.eclipse.ui.application.IWorkbenchWindowConfigurer;
import org.eclipse.ui.application.WorkbenchWindowAdvisor;

/**
 * Configures the WorkbenchWindow for the execution of the Application.
 * @author jkowald
 */
public class ApplicationWorkbenchWindowAdvisor extends WorkbenchWindowAdvisor {

    public ApplicationWorkbenchWindowAdvisor(IWorkbenchWindowConfigurer configurer) {
        super(configurer);
    }

    @Override
	public ActionBarAdvisor createActionBarAdvisor(IActionBarConfigurer configurer) {
        return new ApplicationActionBarAdvisor(configurer);
    }
    
    @Override
	public void preWindowOpen() {
        IWorkbenchWindowConfigurer configurer = getWindowConfigurer();
        configurer.setInitialSize(new Point(1600, 1000));
        configurer.setShowCoolBar(false);
        configurer.setShowStatusLine(false);
        configurer.setShowProgressIndicator(false);
        configurer.setShowFastViewBars(false);
        configurer.setTitle("CARiSMA Rule Allocator");
    }

}
