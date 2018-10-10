package carisma.ui.eclipse.editors.descriptions;

import java.util.List;

import org.eclipse.core.resources.IFile;

import carisma.ui.eclipse.editors.AbstractEditorDescriptor;
import carisma.ui.eclipse.preferences.Constants;


/**Replaced with carisma.ui.eclipse.descriptor 
 * 
 * Extends the AbstractEditorDescriptor for BPNM2 Visual Editor.
 */
@Deprecated
public class BPMN2EditorDescriptor extends AbstractEditorDescriptor {
	
	/**
	 * Getter for the EditorDescriptor name.
	 * @return the name of the descriptor
	 */
	public static final String NAME = "BPMN_EDITOR";
	
	@Override
	public final String getName() {
		return NAME;
	}

	@Override
	public final String getId() {
		return Constants.BPMN2_EDITOR_ID;
	}

	@Override
	public final boolean isApplicable(final IFile modelIFile) {
		if (modelIFile != null 
				&& (modelIFile.getFileExtension().equalsIgnoreCase("bpmn2") ||
						modelIFile.getFileExtension().equalsIgnoreCase("bpmn"))
				&& modelIFile.exists()) {
			return true;
		}
		return false;
	}

	@Override
	public void setName(String name) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setID(String id) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTypes(List<String> types) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setExtension(String extension) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public List<String> getTypes() {
		// TODO Auto-generated method stub
		return null;
	}

}
