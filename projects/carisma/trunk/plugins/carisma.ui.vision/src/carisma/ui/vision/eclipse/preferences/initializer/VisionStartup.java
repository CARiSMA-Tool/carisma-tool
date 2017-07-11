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
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
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
		final Job job = new Job("Connect to VisiOn Launcher") {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				boolean success = getDataFromVisionLauncher();

				final Display display = Display.getDefault();

				if (!success) {
					display.asyncExec(new Runnable() {

						@Override
						public void run() {
							Shell shell = new Shell(display);

							MessageBox dialog = new MessageBox(shell, SWT.ICON_ERROR);
							dialog.setText("Error");
							dialog.setMessage("Error CARiSMA couldn't connect to VisiOn launcher");
						}
					});

					return new Status(Status.ERROR, VisionActivator.PLUGIN_ID,
							"Error CARiSMA couldn't connect to VisiOn launcher");
				} 

				return Status.OK_STATUS;
			}
		};
		job.schedule();
		
		Job getProjectJob = new Job("Get VisiOn project from VisiOn DB"){

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				final Display display = Display.getDefault();
				try{
					job.join();
				}
				catch(Exception e){
					return Status.CANCEL_STATUS;
				}
				if(job.getResult() == Status.OK_STATUS){
					
					if (!restoreVisiOnProject()) {
						display.asyncExec(new Runnable() {
							
							@Override
							public void run() {
								Shell shell = new Shell(display);
								MessageBox dialog = new MessageBox(shell, SWT.ICON_ERROR);
								dialog.setText("Error");
								dialog.setMessage("CARiSMA couldn't restore project from DB");
							}
						});
						return new Status(Status.ERROR, VisionActivator.PLUGIN_ID,
								"CARiSMA couldn't restore project from DB");
					}
					return Status.OK_STATUS;
				}
				return Status.CANCEL_STATUS;
			}
		};
		getProjectJob.schedule();
	}

	protected static boolean restoreVisiOnProject() {
		Content content;
		try {
			content = getProjectFromDB();
		} catch (VisionLauncherException e3) {
			e3.printStackTrace();
			return false;
		}

		String name = CarismaGUI.INSTANCE.getPreferenceStore().getString(VisiOn.PROJECT_NAME);

		if (name == null || name == "") {
			new VisionInitializer().initializeDefaultPreferences();
			name = CarismaGUI.INSTANCE.getPreferenceStore().getString(VisiOn.PROJECT_NAME);
			System.err.println("Initilized VisiOn default preferences.");
		}

		ZipFile zipFile = null;

		// search/create project
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IProject newProject = workspace.getRoot().getProject(name);
		if (newProject.exists()) {
			if (content == null) {
				// There is no project in the DB but there is a local project
				// with the VisiOn project name => do nothing
				return true;
			} else {
				// There is a project in the DB and a local project => import DB
				// project with unused name and inform user

				try {
					
					// Search for unused Name
					newProject = workspace.getRoot().getProject(name + "_DB");
					int i = 1;
					while (newProject.exists()) {
						newProject = workspace.getRoot().getProject(name + "_DB_"+i++);
					}
					zipFile = createTemporaryZip(content);

					final String message = "Your workspace already contains a project with the name \"" + name
					+ "\". A new project from the VisiOn database has been imported as \"" + newProject.getName()
					+ "\".\n\n At closing of Eclipse the project with the name \"" + name
					+ "\" will be uploaded into the VisiOn database.\n\n"
					+ "Please work on the project \""+name+"\", "
					+ "rename the edited projects to \""+ name + "\""
					+ " or change the VisiOn properties to avoid loss of data.";
					
					asyncShowMessage(message);
				} catch (IOException | JSONException e) {
					e.printStackTrace();
					return false;
				}

			}

		} else {
			if (content == null) {
				// There is no project in the DB and no project in the workspace
				// => import DummyProjectfrom plugin and inform the user
				try {
					InputStream s = Platform.getBundle(VisionActivator.PLUGIN_ID)
							.getResource("emptyModel/VisiOnDummy.zip").openConnection().getInputStream();
					File tempFile = File.createTempFile("carisma_vision_project", ".zip");
					Files.copy(s, Paths.get(tempFile.toURI()), StandardCopyOption.REPLACE_EXISTING);
					zipFile = new ZipFile(tempFile);
					

					asyncShowMessage("A new VisiOn project with the name \"" + newProject.getName() + "\"has been created."
							+ " At closing of Eclipse this project will be uploaded to the VisiOn database.");
				} catch (IOException e) {
					e.printStackTrace();
					return false;
				}
			} else {
				try {
					zipFile = createTemporaryZip(content);
				} catch (IOException | JSONException e) {
					e.printStackTrace();
					return false;
				}
			}
		}

		return importProjectFromZIP(newProject, zipFile);
	}

	private static void asyncShowMessage(final String message) {
		final Display display = Display.getDefault();
		display.syncExec(new Runnable() {

			@Override
			public void run() {
				Shell shell = new Shell(display);
				MessageBox dialog = new MessageBox(shell, SWT.ICON_INFORMATION);
				dialog.setText("Information");
				dialog.setMessage(message);
				dialog.open();
			}
		});
	}

	private static ZipFile createTemporaryZip(Content content) throws IOException, JSONException {
		ZipFile zipFile;
		File tempFile = File.createTempFile("carisma_vision_project", ".zip");

		// There is no Project in the workspace but a project in the DB =>
		// import the DB project
		if (content.getFormat() != "JSON") {
			throw new RuntimeException("No JSON-File!");
		}
		JSON json = (JSON) content;
		Object carisma = json.get("carisma");
		Files.write(tempFile.toPath(), Base64.decodeBase64((String) carisma));

		// create a zipFile from the tempFile

		zipFile = new ZipFile(tempFile);
		return zipFile;
	}

	private static boolean importProjectFromZIP(IProject newProject, ZipFile zipFile) {
		IOverwriteQuery overwriteQuery = new IOverwriteQuery() {
			public String queryOverwrite(String file) {
				return ALL;
			}
		};

		// import the zipFile as a new project
		ZipLeveledStructureProvider provider = new ZipLeveledStructureProvider(zipFile);
		Enumeration<? extends ZipEntry> entries = zipFile.entries();
		List<Object> fileSystemObjects = new ArrayList<Object>();
		while (entries.hasMoreElements()) {
			fileSystemObjects.add((Object) entries.nextElement());
		}
		IProjectDescription newProjectDescription = newProject.getWorkspace()
				.newProjectDescription(newProject.getName());

		try {
			newProject.create(newProjectDescription, null);
			newProject.open(null);
		} catch (CoreException e) {
			e.printStackTrace();
			return false;
		}

		ImportOperation importOperation = new ImportOperation(newProject.getFullPath(),
				new ZipEntry(newProject.getName()), provider, overwriteQuery, fileSystemObjects);
		importOperation.setCreateContainerStructure(false);
		try {
			importOperation.run(new NullProgressMonitor());
		} catch (InvocationTargetException | InterruptedException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}

	private static Content getProjectFromDB() throws VisionLauncherException {
		// read file from DB
		PreferencesObject preferencesStore = null;
		preferencesStore = VisionActivator.getINSTANCE().getVisionPreferences();
		Map<String, Object> map = preferencesStore.getObject();

		String user = (String) map.get(PreferencesConstants.dbuser.toString());
		String secret = (String) map.get(PreferencesConstants.dbpasswd.toString());
		String url = (String) map.get(PreferencesConstants.dbaddress.toString());
		int port = Integer.valueOf((String) map.get(PreferencesConstants.dbport.toString())).intValue();

		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url, port);

		String visionCollection = (String) map.get(PreferencesConstants.vision_collection.toString());
		String carismaDocument = (String) map.get(PreferencesConstants.carisma_document.toString());

		MongoDBDestination config = new MongoDBDestination(visionCollection, carismaDocument, null); // TODO:

		Content content = db.read(config);
		return content;
	}

	protected static boolean getDataFromVisionLauncher() {
		boolean error = false;

		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String id = preferencesStore.getString(VisiOn.LAUNCHER_CARISMA_ID);
		int port = preferencesStore.getInt(VisiOn.LAUNCHER_PORT);
		String passwd = preferencesStore.getString(VisiOn.LAUNCHER_PASSWD);

		if (id == null || id.length() == 0 || port < 1 || passwd == null || passwd.length() == 0) {
			asyncShowMessage("Error CARiSMA VisiOn launcher is not configured.");
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
