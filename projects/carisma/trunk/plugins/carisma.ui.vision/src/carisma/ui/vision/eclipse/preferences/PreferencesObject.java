package carisma.ui.vision.eclipse.preferences;

import java.security.GuardedObject;
import java.util.Map;

public final class PreferencesObject extends GuardedObject {

	/**
	 * 
	 */
	private static final long serialVersionUID = -4316478868227190069L;

	public PreferencesObject(Map<String, Object> preferences, PreferencesGuard guard) {
		super(preferences, guard);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public final Map<String, Object> getObject(){
		return (Map<String, Object>) super.getObject();
	}

}
