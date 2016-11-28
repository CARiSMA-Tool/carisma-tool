package carisma.bpmn2.marisk;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class BPMN2MariskActivator extends Plugin {

	public BPMN2MariskActivator INSTANCE;

	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		
		this.INSTANCE = this;
	}
	
}
