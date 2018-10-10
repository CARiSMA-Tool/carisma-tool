package $packageName$;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PathEditor;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;
import $pluginClass$;


/** Handles the Preference Page.
 */

public class $MainClassPage$ extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	/** Constructor sets the PreferenceStore and a Description.
	 */
	public $MainClassPage$() {
		super(GRID);
		setPreferenceStore($activator$.getDefault().getPreferenceStore());
		setDescription("This is a sample preference page.");
	}
	
	/** 
	 * The easiest way is to use FildEditors. As they do all the store and restore work for you.
	 * Further informations can be found at http://www.eclipse.org/articles/Article-Field-Editors/field_editors.html
	 */
	
	public final void createFieldEditors() {
		addField(new StringFieldEditor(
				PreferenceConstants.TEXT_LINE, 
				"Some text preference", 
				getFieldEditorParent()));
		
		addField(new BooleanFieldEditor(
				PreferenceConstants.DECISION,
				"True or False?",
				getFieldEditorParent()));
		
		addField(new IntegerFieldEditor(
				PreferenceConstants.NUMBER, 
				"Select a number", 
				getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(
				PreferenceConstants.MUTUAL,
				"Which option should it be",
				1,
				new String[][]{{"Working example", "good"}, {"Not working example", "bad"}},
				getFieldEditorParent()));
		
		addField(new ColorFieldEditor(
				PreferenceConstants.COLOR,
				"Select a color",
				getFieldEditorParent()));
		
		addField(new FileFieldEditor(
				PreferenceConstants.FILE_NAME,
				"Select a file.",
				getFieldEditorParent()));
		
		addField(new PathEditor(
				PreferenceConstants.PATH_NAME,
				"Related Paths.",
				"If there are different paths to look at, add them here",
				getFieldEditorParent()));


}
	
	@Override	
	public void init(final IWorkbench workbench) {
		
	}

}
