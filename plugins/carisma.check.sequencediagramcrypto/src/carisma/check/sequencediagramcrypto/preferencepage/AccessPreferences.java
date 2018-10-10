package carisma.check.sequencediagramcrypto.preferencepage;

import java.awt.Color;

import org.eclipse.jface.preference.IPreferenceStore;

import carisma.check.sequencediagramcrypto.Activator;

/**
 * A Sample Class to demonstrate how to access the preferences.
 */
public class AccessPreferences {
	
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
		return new Color(new Integer(pref[0]), new Integer(pref[1]), new Integer(pref[2]));
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
			e.printStackTrace();
			System.err.println("NO Path set");
		}
		return p;
	}
	
}
