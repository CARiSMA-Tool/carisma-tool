package carisma.profile.umlsec.umlsec4ids;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import carisma.core.Carisma;
import carisma.core.logging.LogLevel;
import carisma.core.logging.Logger;
/**
 * The activator class controls the plug-in life cycle
 */
public class Activator implements BundleActivator {

	@Override
	public void start(BundleContext context) throws Exception {
		try {
			Carisma.getInstance().getModelManager().addMapping("umlsec4ids.profile.uml", "platform:/plugin/carisma.profile.umlsec.umlsec4ids/profile/umlsec4ids.profile.uml");
		} catch (Exception e) {
			Logger.log(LogLevel.ERROR, "Error while putting profile path to map", e);
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub

	}

}
