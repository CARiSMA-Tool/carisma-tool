package carisma.ui.vision.eclipse.preferences.initializer;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Map;

import javax.swing.JOptionPane;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
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
		 Job job = new Job("Connect to VisiOn Launcher") {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				boolean success = getDataFromVisionLauncher();
				
				if(!success){
					JOptionPane.showMessageDialog(null,"Error CARiSMA couldn't connect to VisiOn launcher","CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
				}
				
				return Status.OK_STATUS;
			}
		};
		job.schedule();
	}

	public static boolean getDataFromVisionLauncher() {
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
				System.out.println("VISION: Try to connect");
				client.connect();
				System.out.println("VISION: Connected");
				Map<String, Object> preferences = client.getPreferencesMap();
				System.out.println("VISION: Got Map");	
				PreferencesObject preferencesObject = new PreferencesObject(preferences, new PreferencesGuard());
				System.out.println("VISION: Created Preferences Object");
				VisionActivator.getINSTANCE().setVisionPreferences(preferencesObject);
				System.out.println("VISION: Set preferences in Activator");
			}	
		} 
		catch (Exception e) {
			e.printStackTrace();
			error = true;
		}
		return !error;
	}

}
