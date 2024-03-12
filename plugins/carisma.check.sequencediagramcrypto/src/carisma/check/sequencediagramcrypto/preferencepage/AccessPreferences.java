package carisma.check.sequencediagramcrypto.preferencepage;

import java.awt.Color;

import org.eclipse.jface.preference.IPreferenceStore;

import carisma.check.sequencediagramcrypto.Activator;

import java.util.logging.Logger;

/**
 * A Sample Class to demonstrate how to access the preferences.
 */
public class AccessPreferences {
	
	private static final Logger logger = Logger.getLogger(AccessPreferences.class.getName());
	/**
	 * Returns a String Preference.
	 * 
	 * @return the preference TEXT_LINE
	 */
	public final String getStringPreference() {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String pref = store.getString(PreferenceConstants.TEXT_LINE);
		
		return pref;
	}
	
	/**
	 * Returns a Color Preference.
	 * 
	 * @return the preference COLOR
	 */
	public final Color getColorPreference() {
		
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		String[] pref = store.getString(PreferenceConstants.COLOR).split(",");
		return new Color(Integer.parseInt(pref[0]), Integer.parseInt(pref[1]), Integer.parseInt(pref[2]));
	}
	
	/**
	 * Returns an array of Paths.
	 * 
	 * @return the preference PATH_NAME
	 */
	public final String[] getPathPreference() {
		String[] p = null;
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		try {
			p = store.getString(PreferenceConstants.PATH_NAME).split(";");
		}
		catch (NullPointerException e) {
			logger.warning("Error message: " + e.getMessage());
			System.err.println("NO Path set");
		}
		return p;
	}
	
}
