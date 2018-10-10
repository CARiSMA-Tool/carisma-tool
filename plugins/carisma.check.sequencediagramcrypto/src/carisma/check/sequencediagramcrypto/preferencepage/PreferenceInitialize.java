package carisma.check.sequencediagramcrypto.preferencepage;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import carisma.check.sequencediagramcrypto.Activator;

/**
 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer
 */
public class PreferenceInitialize extends AbstractPreferenceInitializer {
	
	@Override
	public final void initializeDefaultPreferences() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		
		store.setDefault(PreferenceConstants.TEXT_LINE, "Enter your text here.");
	}
	
}
