package carisma.ui.vision.eclipse.preferences.initializer;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;

import javax.swing.JOptionPane;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.IStartup;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.VisionActivator;
import carisma.ui.vision.eclipse.preferences.PreferencesGuard;
import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.eclipse.preferences.pages.VisiOn;
import it.unitn.disi.ststool.vision.integration.preferences.VisonPreferencesClient;

public class VisionStartup implements IStartup {

	private static InetAddress LOCAL_HOST;
	
	public VisionStartup() throws UnknownHostException {
		VisionStartup.LOCAL_HOST = InetAddress.getLocalHost();
	}

	@Override
	public void earlyStartup() {
		boolean error = getDataFromVisionLauncher();
		
		if(error){
			JOptionPane.showMessageDialog(null,"Error CARiSMA couldn't connect to VisiOn launcher","CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
		}
	}

	private boolean getDataFromVisionLauncher() {
		boolean error = false;
		
		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String id = preferencesStore.getString(VisiOn.LAUNCHER_CARISMA_ID);
		int port = preferencesStore.getInt(VisiOn.LAUNCHER_PORT);
		String passwd = preferencesStore.getString(VisiOn.LAUNCHER_PASSWD);
		
		if(id == null || id.length() == 0 || port < 1 || passwd == null || passwd.length() == 0){
			JOptionPane.showMessageDialog(null,"Error CARiSMA VisiOn launcher is not configured.","CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
			return false;
		}
		
		try {
			try(VisonPreferencesClient client = new VisonPreferencesClient(id, LOCAL_HOST, port, passwd)){
				client.connect();
				Map<String, Object> preferences = client.getPreferencesMap();
				PreferencesObject preferencesObject = new PreferencesObject(preferences, new PreferencesGuard());
				
				VisionActivator.getINSTANCE().setVisionPreferences(preferencesObject);
			}
			
		} catch (UnknownHostException e) {
			//e.printStackTrace();
			error = true;
		} catch (Exception e) {
			e.printStackTrace();
			error = true;
		}
		return !error;
	}

}
