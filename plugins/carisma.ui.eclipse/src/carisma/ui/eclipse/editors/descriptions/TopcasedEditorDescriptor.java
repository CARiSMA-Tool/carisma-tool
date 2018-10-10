/*******************************************************************************
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 *******************************************************************************/
package carisma.ui.eclipse.editors.descriptions;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import carisma.ui.eclipse.editors.AbstractEditorDescriptor;

/**
 * Extends the AbstractEditorDescriptor for Topcased.
 */
@Deprecated
public class TopcasedEditorDescriptor extends AbstractEditorDescriptor {

	/**
	 * EditorDescriptor name.
	 */
	public static final String NAME = "TOPCASED";
	
	//########################################################################################
	/**
	 * Constructor.
	 */
	public TopcasedEditorDescriptor() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * Getter for the EditorDescriptor name.
	 * @return the name of the descriptor
	 */
	@Override
	public final String getName() {
		return NAME;
	}

	/**
	 * Getter for the EditorDescriptor id.
	 * @return the Id of the descriptor
	 */
	@Override
	public final String getId() {
		return "org.topcased.modeler.uml.editor.UMLEditor";
	}

	/**
	 * Check if EditorDescriptor is applicable for <code>file</code>.
	 * @param modelIFile the model IFile
	 * @return boolean
	 */
	@Override
	public final boolean isApplicable(final IFile modelIFile) {

		IFile f = getGraphicalFile(modelIFile);
		
		if (f != null && f.exists()) {
			return true;
		}
		return false;
	}

	/**
	 * Opens the IFile.
	 * @param modelIFile the model IFile
	 * @return boolean
	 */
	@Override
	public final boolean forceOpen(final IFile modelIFile) {
				
		return super.forceOpen(getGraphicalFile(modelIFile));
	}

	/**
	 * Returns the corresponding ".umldi"-File for a model file.
	 * @param modelIFile the corresponding topcased graphic file
	 * @return IFile
	 */
	private static IFile getGraphicalFile(final IFile modelIFile) {
		if (!modelIFile.getFileExtension().equalsIgnoreCase("uml")) {
			return null;
		}
		String fileWithoutExt = modelIFile.getFullPath().toOSString().substring(0,
				modelIFile.getFullPath().toOSString().length() - 4);
		IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		IPath path = Path.fromOSString(fileWithoutExt + ".umldi");
		IFile grIfile = workspaceRoot.getFile(path);
		
		return grIfile;
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
