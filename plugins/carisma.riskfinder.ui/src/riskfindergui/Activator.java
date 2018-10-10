package riskfindergui;

import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The activator class controls the plug-in life cycle.
 */
public class Activator extends AbstractUIPlugin {

	/**
	 *  The plug-in ID.
	 */
	public static final String PLUGIN_ID = "riskfindergui"; //$NON-NLS-1$

	/**
	 *  The shared instance.
	 */
	private static Activator plugin;
	
	/**
	 * The constructor.
	 */
	public Activator() {
	}

	/**
	 * (non-Javadoc) (jetzt schon!).
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 * @param context The context. //TODO
	 * @throws Exception ??? //TODO
	 **/
	public final void start(final BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/**
	 * (non-Javadoc).
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 * @param context The context. //TODO
	 * @throws Exception ??? //TODO
	 **/
	public final void stop(final BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

}
