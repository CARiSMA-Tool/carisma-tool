package carisma.ui.vision.eclipse.preferences;

import java.security.Guard;

import carisma.ui.vision.eclipse.preferences.initializer.VisionInitializer;

public final class PreferencesGuard implements Guard {

	private static final Class<?>[] allowedClasses = {VisionInitializer.class};
	
	@Override
	public void checkGuard(Object accessingObject) throws SecurityException {
		boolean allowAccess = false;
		
		Class<? extends Object> accessingClass = accessingObject.getClass();
		for(Class<?> allowed : allowedClasses){
			allowAccess |= (allowed == accessingClass);
		}
		
		if(!allowAccess){
			throw new SecurityException();
		}
	}

}
