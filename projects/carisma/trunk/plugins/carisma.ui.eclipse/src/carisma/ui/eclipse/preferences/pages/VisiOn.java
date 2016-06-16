package carisma.ui.eclipse.preferences.pages;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import carisma.ui.eclipse.CarismaGUI;

public class VisiOn extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public static final String KEY_CARISMA_FIELD = "CARiSMA_Field";
	public static final String KEY_CARISMA_DOCUMENT = "CARiSMA_Document";
	public static final String KEY_CARISMA_COLLECTION = "CARiSMA_Collection";
	public static final String KEY_PLA_FIELD = "PLA_Field";
	public static final String KEY_PLA_DOCUMENT = "PLA_Document";
	public static final String KEY_PLA_COLLECTION = "PLA_Collection";
	public static final String KEY_STS_FIELD = "STS_Field";
	public static final String KEY_STS_DOCUMENT = "STS_Document";
	public static final String KEY_STS_COLLECTION = "STS_Collection";
	public static final String KEY_URL = "Url";
	public static final String KEY_SECRET = "Secret";
	public static final String KEY_USER = "User";

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
		StringFieldEditor user = new StringFieldEditor(KEY_USER, "User:\t\t\t\t",
		        getFieldEditorParent());
		addField(user);
		
		StringFieldEditor secret = new StringFieldEditor(KEY_SECRET, "Secret:\t\t\t\t",
		        getFieldEditorParent());
		addField(secret);
		
		StringFieldEditor url = new StringFieldEditor(KEY_URL, "Url:\t\t\t\t",
		        getFieldEditorParent());
		addField(url);
		
		/*
		 * Location of STS input
		 */
		StringFieldEditor sts_collection = new StringFieldEditor(KEY_STS_COLLECTION, "Collection of STS input:\t\t",
		        getFieldEditorParent());
		addField(sts_collection);
		
		StringFieldEditor sts_document = new StringFieldEditor(KEY_STS_DOCUMENT, "Document of STS input:\t\t",
		        getFieldEditorParent());
		addField(sts_document);
		
		StringFieldEditor sts_field = new StringFieldEditor(KEY_STS_FIELD, "Field of STS input:\t\t\t",
		        getFieldEditorParent());
		addField(sts_field);
		
		/*
		 * Location of PLA Html output
		 */
		StringFieldEditor pla_collection = new StringFieldEditor(KEY_PLA_COLLECTION, "Collection of PLA output:\t\t",
		        getFieldEditorParent());
		addField(pla_collection);
		
		StringFieldEditor pla_document = new StringFieldEditor(KEY_PLA_DOCUMENT, "Document of PLA output:\t\t",
		        getFieldEditorParent());
		addField(pla_document);
		
		StringFieldEditor pla_field = new StringFieldEditor(KEY_PLA_FIELD, "Field of PLA output:\t\t\t",
		        getFieldEditorParent());
		addField(pla_field);
		
		/*
		 * Location of CARiSMA XML output
		 */
		StringFieldEditor carisma_collection = new StringFieldEditor(KEY_CARISMA_COLLECTION, "Collection of CARiSMA XML output:\t",
		        getFieldEditorParent());
		carisma_collection.setEmptyStringAllowed(false);
		addField(carisma_collection);
		
		StringFieldEditor carisma_document = new StringFieldEditor(KEY_CARISMA_DOCUMENT, "Document of CARiSMA XML output:\t",
		        getFieldEditorParent());
		carisma_document.setEmptyStringAllowed(false);
		addField(carisma_document);
		
		StringFieldEditor carisma_field = new StringFieldEditor(KEY_CARISMA_FIELD, "Field of CARiSMA XML output:\t\t",
		        getFieldEditorParent());
		carisma_field.setEmptyStringAllowed(true);
		addField(carisma_field);
	}

}
