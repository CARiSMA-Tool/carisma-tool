package $packageName$;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import $mainPackageName$.$activator$;

/** @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer
 */
public class PreferenceInitialize extends AbstractPreferenceInitializer {

	@Override
	public final void initializeDefaultPreferences() {
		IPreferenceStore store = $activator$.getDefault().getPreferenceStore();
		
		store.setDefault(PreferenceConstants.TEXT_LINE, "Enter your text here.");
	}
	
	
}



