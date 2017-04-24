package carisma.ui.vision.eclipse.preferences.initializer;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import javax.swing.JOptionPane;
import org.apache.commons.codec.binary.Base64;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.dialogs.IOverwriteQuery;
import org.eclipse.ui.internal.wizards.datatransfer.ZipLeveledStructureProvider;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;
import org.json.JSONException;

import carisma.core.io.content.Content;
import carisma.core.io.content.JSON;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.VisionActivator;
import carisma.ui.vision.eclipse.preferences.PreferencesConstants;
import carisma.ui.vision.eclipse.preferences.PreferencesGuard;
import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.eclipse.preferences.pages.VisiOn;
import carisma.ui.vision.exceptions.VisionLauncherException;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;
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

				if (!success) {
					JOptionPane.showMessageDialog(null, "Error CARiSMA couldn't connect to VisiOn launcher",
							"CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
				}
				else {
					if(!getProjectFromDB()){
						JOptionPane.showMessageDialog(null, "Error CARiSMA couldn't restore project from DB",
								"CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
					}
				}

				return Status.OK_STATUS;
			}
		};
		job.schedule();
	}
	
	protected static boolean getProjectFromDB() {
		// read file from DB
		PreferencesObject preferencesStore = null;
		try {
			preferencesStore = VisionActivator.getINSTANCE().getVisionPreferences();
		} catch (VisionLauncherException e) {
			e.printStackTrace();
			return false;
		}
		Map<String, Object> map = preferencesStore.getObject();

		String user = (String) map.get(PreferencesConstants.dbuser.toString());
		String secret = (String) map.get(PreferencesConstants.dbpasswd.toString());
		String url = (String) map.get(PreferencesConstants.dbaddress.toString());

		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);

		String visionCollection = (String) map.get(PreferencesConstants.vision_collection.toString());
		String carismaDocument = (String) map.get(PreferencesConstants.carisma_document.toString());
		
		MongoDBDestination config = new MongoDBDestination(visionCollection, carismaDocument, null); // TODO:
			
		Content content = db.read(config);

		//tempfile for the project
		String name = CarismaGUI.INSTANCE.getPreferenceStore().getString(VisiOn.PROJECT_NAME);

		if(name == null || name == ""){
			new VisionInitializer().initializeDefaultPreferences();
			name = CarismaGUI.INSTANCE.getPreferenceStore().getString(VisiOn.PROJECT_NAME);
		}
			
		String prefix = name;
		String suffix = ".zip";
		
		File tempFile = null;
		try {
			tempFile = File.createTempFile(prefix, suffix);
		} catch (IOException e1) {
			e1.printStackTrace();
			return false;
		}
		//search/create project
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IProject newProject = workspace.getRoot().getProject(name);
		ZipFile zipFile = null;

		//tests if project already exists
		if (newProject.exists()) {
			
			Display display = new Display();
			Shell shell = new Shell(display);
			
			MessageBox dialog = new MessageBox(shell, SWT.ICON_INFORMATION);
			dialog.setText("Information");
			dialog.setMessage("Your workspace already contains a project with the name " + name + " and a new project from the VisiOn database has been imported as " + name + "_DB. At closing of Eclipse the project with the name " + name + " will be uploaded into the VisiOn database. Please ensure that you edit the right project and rename it accordingly to " + name + " or change the VisiOn properties to avoid loss of data.");
			
			// open dialog and await user selection
			int returnCode = dialog.open();
			
			name = name + "_DB";
			newProject = workspace.getRoot().getProject(name);
			
		} 
			//tests if a real content has been read from DB
			if (content == null) {
				//read a zipFile in the tempFile
				try {
					InputStream s = Platform.getBundle(VisionActivator.PLUGIN_ID).getResource("emptyModel/VisiOnDummy.zip").openConnection().getInputStream();
					Files.copy(s, Paths.get(tempFile.toURI()), StandardCopyOption.REPLACE_EXISTING);
				} catch (IOException e) {
					e.printStackTrace();
					return false;
				}
			} else {
				//write the content in the tempFile
				if(content.getFormat() != "JSON"){
					throw new RuntimeException("No JSON-File!");
				}
				JSON json = (JSON) content;
				try {
					Object carisma = json.get("carisma");
					Files.write(tempFile.toPath(), Base64.decodeBase64((String) carisma));
				} catch (JSONException e2) {
					e2.printStackTrace();
					return false;
				} catch (IOException e) {
					e.printStackTrace();
					return false;
				}
			}
		

		IOverwriteQuery overwriteQuery = new IOverwriteQuery() {
			public String queryOverwrite(String file) {
				return ALL;
			}
		};
			
		//create a zipFile from the tempFile
		try {
			zipFile = new ZipFile(tempFile);
		} catch (IOException e1) {
			e1.printStackTrace();
			return false;
		}

		//import the zipFile as a new project
		ZipLeveledStructureProvider provider = new ZipLeveledStructureProvider(zipFile);
		Enumeration<? extends ZipEntry> entries = zipFile.entries();
		List<Object> fileSystemObjects = new ArrayList<Object>();
		while (entries.hasMoreElements()) {
			fileSystemObjects.add((Object) entries.nextElement());
		}
		IProjectDescription newProjectDescription = workspace.newProjectDescription(name);

		try {
			newProject.create(newProjectDescription, null);
			newProject.open(null);
		} catch (CoreException e) {
			e.printStackTrace();
			return false;
		}

		ImportOperation importOperation = new ImportOperation(newProject.getFullPath(),
			new ZipEntry(name), provider, overwriteQuery, fileSystemObjects);
			importOperation.setCreateContainerStructure(false);
		try {
			importOperation.run(new NullProgressMonitor());
		} catch (InvocationTargetException | InterruptedException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	protected static boolean getDataFromVisionLauncher() {
		boolean error = false;

		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String id = preferencesStore.getString(VisiOn.LAUNCHER_CARISMA_ID);
		int port = preferencesStore.getInt(VisiOn.LAUNCHER_PORT);
		String passwd = preferencesStore.getString(VisiOn.LAUNCHER_PASSWD);

		if (id == null || id.length() == 0 || port < 1 || passwd == null || passwd.length() == 0) {
			JOptionPane.showMessageDialog(null, "Error CARiSMA VisiOn launcher is not configured.",
					"CARiSMA VisiOn Error", JOptionPane.OK_OPTION);
			return false;
		}

		try {
			try (VisonPreferencesClient client = new VisonPreferencesClient(id, LOCAL_HOST, port, passwd)) {
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
		} catch (Exception e) {
			e.printStackTrace();
			error = true;
		}
		return !error;
	}

}
