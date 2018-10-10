package carisma.ui.vision.eclipse.preferences.pages;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import carisma.ui.eclipse.CarismaGUI;

public class VisiOn extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public static final String LAUNCHER_PORT 		= "LAUNCHER_PORT" ;
	public static final String LAUNCHER_CARISMA_ID 	= "LAUNCHER_CARISMA_ID";
	public static final String LAUNCHER_PASSWD 		= "LAUNCHER_PASSWD";
	public static final String PROJECT_NAME 		= "DB_PROJECT";

	@Override
	public void init(IWorkbench workbench) {
		setPreferenceStore(CarismaGUI.INSTANCE.getPreferenceStore());
	    setDescription("VisiOn Database Preferences");
	}

	@Override
	protected void createFieldEditors() {
		/*
		 * General configuration
		 */
		StringFieldEditor id = new StringFieldEditor(LAUNCHER_CARISMA_ID, "Launcher ID:\t\t\t\t",
		        getFieldEditorParent());
		addField(id);
		
		StringFieldEditor secret = new StringFieldEditor(LAUNCHER_PASSWD, "Secret:\t\t\t\t",
		        getFieldEditorParent());
		addField(secret);
		
		IntegerFieldEditor port = new IntegerFieldEditor(LAUNCHER_PORT, "Port:\t\t\t\t",
		        getFieldEditorParent());
		addField(port);
		
		StringFieldEditor project = new StringFieldEditor(PROJECT_NAME, "Project Name:\t\t\t",
		        getFieldEditorParent());
		addField(project);
	}
}
