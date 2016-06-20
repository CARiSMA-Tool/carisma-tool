package carisma.ui.eclipse.preferences.initializer;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.eclipse.preferences.pages.VisiOn;

public class VisiOnInitializer extends AbstractPreferenceInitializer {

	public VisiOnInitializer() {
	}

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();		
		
		preferencesStore.setDefault(VisiOn.KEY_URL, "212.34.151.216");
		
		preferencesStore.setDefault(VisiOn.KEY_CARISMA_COLLECTION, "testCollection");
		preferencesStore.setDefault(VisiOn.KEY_CARISMA_DOCUMENT, "CARiSMA");
		preferencesStore.setDefault(VisiOn.KEY_CARISMA_FIELD, "");
		
		preferencesStore.setDefault(VisiOn.KEY_STS_COLLECTION, "testCollection");
		preferencesStore.setDefault(VisiOn.KEY_STS_DOCUMENT, "sts_model");
		preferencesStore.setDefault(VisiOn.KEY_STS_FIELD, "srs");
		
		preferencesStore.setDefault(VisiOn.KEY_PLA_FIELD, "trustAnalysis");
	}

}
