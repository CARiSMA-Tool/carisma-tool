package carisma.ui.vision;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.exceptions.VisionLauncherException;

public class VisionActivator extends Plugin {
	
	public static final String PLUGIN_ID = "carisma.ui.vision";
	
	private static VisionActivator INSTANCE;
	
	private PreferencesObject preferencesObject;
	
	public PreferencesObject getVisionPreferences() throws VisionLauncherException{
		if(this.preferencesObject == null){
			throw new VisionLauncherException("No data received from launcher.");
		}
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

	public static VisionActivator getINSTANCE() {
		return INSTANCE;
	}
	
	public boolean isDBAccessible(){
		return this.preferencesObject != null;
	}
}
