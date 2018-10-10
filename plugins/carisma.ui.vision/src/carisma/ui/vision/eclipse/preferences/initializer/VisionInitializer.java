package carisma.ui.vision.eclipse.preferences.initializer;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.eclipse.preferences.pages.VisiOn;

public class VisionInitializer extends AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = CarismaGUI.INSTANCE.getPreferenceStore();
		store.setDefault(VisiOn.LAUNCHER_CARISMA_ID, "carisma");
		store.setDefault(VisiOn.LAUNCHER_PASSWD, "vision");
		store.setDefault(VisiOn.LAUNCHER_PORT, 9898);
		store.setDefault(VisiOn.PROJECT_NAME, "VisionProject");
	}

}
