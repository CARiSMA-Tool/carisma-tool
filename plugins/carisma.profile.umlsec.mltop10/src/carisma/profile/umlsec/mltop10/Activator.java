package carisma.profile.umlsec.mltop10;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;

public class Activator implements BundleActivator {

	@Override
	public void start(BundleContext context) throws Exception {
		try {
			Carisma.getInstance().getModelManager().addMapping("mltop10.profile.uml",
					"platform:/plugin/carisma.profile.umlsec.mltop10/profile/mltop10.profile.uml");
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Error while putting profile path to map", e);
		}

	}

	@Override
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub

	}

}
