package carisma.ui.vision;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import carisma.ui.vision.eclipse.preferences.PreferencesObject;

public class VisionActivator extends Plugin {
	
	public static final String PLUGIN_ID = "carisma.ui.vision";
	
	public static VisionActivator INSTANCE;
	
	private PreferencesObject preferencesObject;
	
	public PreferencesObject getVisionPreferences(){
		return this.preferencesObject;
	}
	
	public void setVisionPreferences(PreferencesObject preferencesObject){
		this.preferencesObject = preferencesObject;
	}
	
	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		INSTANCE = this;
	}
}
