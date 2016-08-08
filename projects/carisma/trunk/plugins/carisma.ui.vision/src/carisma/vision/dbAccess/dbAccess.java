package carisma.vision.dbAccess;

import java.io.File;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.w3c.dom.Document;

import carisma.core.io.content.Content;
import carisma.core.io.content.ContentFactory;
import carisma.core.io.implementations.FileIO;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBDynamicConfiguration;
import carisma.core.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.eclipse.CarismaGUI;
import static carisma.ui.vision.eclipse.preferences.pages.VisiOn.*;

public class dbAccess {
	public  static Document loadSTSInputFromDB() {
		IPreferenceStore preferencesStore = CarismaGUI.INSTANCE.getPreferenceStore();

		String user = preferencesStore.getString(KEY_USER);
		String secret = preferencesStore.getString(KEY_SECRET);
		String url = preferencesStore.getString(KEY_URL);

		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);

		String stsCollection = preferencesStore.getString(KEY_STS_COLLECTION);
		String stsDocument = preferencesStore.getString(KEY_STS_DOCUMENT);
		String stsField = preferencesStore.getString(KEY_STS_FIELD);

		MongoDBDynamicConfiguration config = new MongoDBDynamicConfiguration(url, stsCollection, stsDocument, stsField);
		Content content = db.read(config);
		return ContentFactory.convertToXmlDom(content).getDocument();
	}

	public static Document loadSTSInputFromFile() {
		Shell activeShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		FileDialog dialog = new FileDialog(activeShell);
		dialog.open();
		String filterPath = dialog.getFilterPath();
		String fileName = dialog.getFileName();
		return FileIO.read(new File(new File(filterPath), fileName));
	}
	
}
