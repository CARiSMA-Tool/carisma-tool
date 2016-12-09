package carisma.ui.vision.eclipse.preferences.pages;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import carisma.ui.eclipse.CarismaGUI;
import carisma.ui.vision.VisionActivator;
import carisma.ui.vision.eclipse.preferences.initializer.VisionStartup;

public class VisiOn extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public static final String LAUNCHER_PORT 		= "LAUNCHER_PORT" ;
	public static final String LAUNCHER_CARISMA_ID 	= "LAUNCHER_CARISMA_ID";
	public static final String LAUNCHER_PASSWD 		= "LAUNCHER_PASSWD";

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
	}
}
