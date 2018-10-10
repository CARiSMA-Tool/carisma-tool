package carisma.ui.vision;

import java.io.File;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import javax.swing.JOptionPane;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.codec.binary.Base64;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.internal.wizards.datatransfer.ArchiveFileExportOperation;
import org.osgi.framework.BundleContext;
import org.w3c.dom.Document;

import carisma.core.io.content.Content;
import carisma.core.io.content.ContentFactory;
import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.eclipse.preferences.PreferencesConstants;
import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.eclipse.preferences.pages.VisiOn;
import carisma.ui.vision.exceptions.VisionLauncherException;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;

public class VisionActivator extends Plugin {

	public static final String PLUGIN_ID = "carisma.ui.vision";

	private static VisionActivator INSTANCE;

	private PreferencesObject preferencesObject;

	public PreferencesObject getVisionPreferences() throws VisionLauncherException {
		if (this.preferencesObject == null) {
			throw new VisionLauncherException("No data received from launcher.");
		}
		return this.preferencesObject;
	}

	public void setVisionPreferences(PreferencesObject preferencesObject) {
		this.preferencesObject = preferencesObject;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		INSTANCE = this;
	}

	public String getStringFromDocument(Document doc) {
		try {
			DOMSource domSource = new DOMSource(doc);
			StringWriter writer = new StringWriter();
			StreamResult result = new StreamResult(writer);
			TransformerFactory tf = TransformerFactory.newInstance();
			Transformer transformer = tf.newTransformer();
			transformer.transform(domSource, result);
			return writer.toString();
		} catch (TransformerException ex) {
			ex.printStackTrace();
			return null;
		}
	}

	@Override
	public void stop(BundleContext context) throws Exception {

		final Display display = Display.getDefault();
		
		final String name = CarismaGUI.INSTANCE.getPreferenceStore().getString(VisiOn.PROJECT_NAME);

		if (name != null && !"".equals(name)) {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			final IProject project = workspace.getRoot().getProject(name);
			if (project.exists()) {
				final AtomicBoolean returnCode = new AtomicBoolean();
				display.syncExec(new Runnable() {
					
					@Override
					public void run() {
						Shell shell = new Shell(display);
						MessageBox dialog = new MessageBox(shell, SWT.ICON_QUESTION | SWT.YES | SWT.NO);
						dialog.setText("Info");
						dialog.setMessage("Do you really want to upload the project \"" + name + "\" into the VisiOn database and to delete it locally?");
						returnCode.set(dialog.open() == SWT.YES);


					}
				});
				// open dialog and await user selection
				if (returnCode.get()) {

					String prefix = name;
					String suffix = ".zip";

					final File tempFile = File.createTempFile(prefix, suffix);

					IProgressMonitor IProgressMonitor = new NullProgressMonitor();
					try {
						List<IResource> list = new ArrayList<IResource>();
						list.add(project);
						list.addAll(Arrays.asList(project.members()));
						ArchiveFileExportOperation export = new ArchiveFileExportOperation(project, tempFile.getPath());
						export.run(IProgressMonitor);
						System.out.println("Status: " + export.getStatus());

					} catch (InvocationTargetException | InterruptedException e) {
						e.printStackTrace();
					} catch (CoreException e) {
						e.printStackTrace();
					}
					byte[] bytes = Files.readAllBytes(tempFile.toPath());
					String base64String = Base64.encodeBase64String(bytes);

					Content content = ContentFactory.createContent("{\"carisma\":\"" + base64String + "\"}");

					PreferencesObject preferencesStore = null;
					try {
						preferencesStore = VisionActivator.getINSTANCE().getVisionPreferences();
					} catch (VisionLauncherException e) {
						e.printStackTrace();
					}
					Map<String, Object> map = preferencesStore.getObject();

					String user = (String) map.get(PreferencesConstants.dbuser.toString());
					String secret = (String) map.get(PreferencesConstants.dbpasswd.toString());
					String url = (String) map.get(PreferencesConstants.dbaddress.toString());
					int port = Integer.valueOf((String) map.get(PreferencesConstants.dbport.toString())).intValue();

					MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url, port);

					String visionCollection = (String) map.get(PreferencesConstants.vision_collection.toString());
					String carismaDocument = (String) map.get(PreferencesConstants.carisma_document.toString());
					// String carismaField = (String)
					// map.get(PreferencesConstants.carisma_field.toString());

					MongoDBDestination config = new MongoDBDestination(visionCollection, carismaDocument, null); // TODO:
																													// config

					if (db.write(config, content)) {
						project.delete(true, null);
					}
				}
				super.stop(context);
				return;
			}

			final AtomicInteger returnCode = new AtomicInteger();
			display.syncExec(new Runnable() {
				
				@Override
				public void run() {Shell shell = new Shell(display);
					MessageBox dialog = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK);
					dialog.setText("Error");
					dialog.setMessage("Error CARiSMA couldn't export a project with the name \""+name+"\" to the VisiOn database.");
					returnCode.set(dialog.open());
				}
			});
			super.stop(context);
		}
	}

	public static VisionActivator getINSTANCE() {
		return INSTANCE;
	}

	public boolean isDBAccessible() {
		return this.preferencesObject != null;
	}
}
