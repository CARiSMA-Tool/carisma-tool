package carisma.vision.dbAccess;

import java.io.File;
import java.util.Map;

import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.w3c.dom.Document;

import carisma.core.io.content.Content;
import carisma.core.io.content.ContentFactory;
import carisma.core.io.implementations.FileIO;
import carisma.ui.vision.VisionActivator;
import carisma.ui.vision.eclipse.preferences.PreferencesConstants;
import carisma.ui.vision.eclipse.preferences.PreferencesObject;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI;
import carisma.ui.vision.io.implementations.db.mongodb.restapi.MongoDBRestAPI.MongoDBDestination;

public class dbAccess {
	public  static Document loadSTSInputFromDB() {
		PreferencesObject preferencesStore = VisionActivator.INSTANCE.getVisionPreferences();
		Map<String, Object> map = preferencesStore.getObject();
		
		String user = (String) map.get(PreferencesConstants.dbuser);
		String secret = (String) map.get(PreferencesConstants.dbpasswd);
		String url = (String) map.get(PreferencesConstants.dbaddress);

		
		MongoDBRestAPI db = new MongoDBRestAPI(user, secret, url);

		String stsCollection = (String) map.get(PreferencesConstants.sts_collection);
		String stsDocument = (String) map.get(PreferencesConstants.sts_document);
		String stsField = (String) map.get(PreferencesConstants.sts_field);

		MongoDBDestination config = new MongoDBDestination(stsCollection, stsDocument, stsField);
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
