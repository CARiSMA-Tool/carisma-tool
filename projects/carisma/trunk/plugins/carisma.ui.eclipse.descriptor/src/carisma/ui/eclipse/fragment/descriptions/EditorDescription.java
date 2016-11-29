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
package carisma.ui.eclipse.fragment.descriptions;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

import carisma.ui.eclipse.editors.AbstractEditorDescriptor;

/** EditorDescription Class.
 * @author bberghoff
 */
public class EditorDescription extends AbstractEditorDescriptor {
	/**
	 * Accepted model types.
	 */
	private List<String> types;
	/**
	 * ID of the editor to open the model.
	 */
	private String editorID = "";
	/**
	 * The shown name of the editor in Preferences.
	 */
	private String name = "";
	
	private String fileExtension = null;
	
	
	public EditorDescription() {
	}
	
	@Override
	public String getName() {
		return this.name;
	}


	@Override
	public String getId() {
		return this.editorID;
	}
	
	@Override
	public final boolean isApplicable(final IFile modelIFile) {
		
		if (this.fileExtension != null) {
			IFile toTest = getAlternativeFile(modelIFile);
			if (toTest == null || !toTest.exists()) {
				return false;
			}
		}
		
		if (modelIFile != null
				&& modelIFile.exists()
				&& !this.types.isEmpty()) {
			for (String type : this.types) {
				if (modelIFile.getFileExtension().toLowerCase().matches(type.toLowerCase())
						//bpmn2 modeler registers only bpmn2 extension, should be fixed soon...
						|| (modelIFile.getFileExtension().equalsIgnoreCase("bpmn") ? "bpmn2".matches(type.toLowerCase()) : false)) {
					return true;
				}
			}
		}
		return false;
	}
	
	@Override
	public final boolean forceOpen(final IFile modelIFile) {
		if (this.fileExtension == null) {
			return super.forceOpen(modelIFile);
		}
		return super.forceOpen(getAlternativeFile(modelIFile));
	}
	
	
	/** Returns a special file for an editor.
	 * For example a .di file for the PapyrusEditor
	 * @param modelIFile
	 * @return
	 */
	private IFile getAlternativeFile(IFile modelIFile) {		
		String fileWithoutExt = modelIFile.getFullPath().toOSString().substring(0,
				modelIFile.getFullPath().toOSString().length() - 4);
		IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		IPath path = Path.fromOSString(fileWithoutExt + "." + this.fileExtension); 
		IFile grIfile = workspaceRoot.getFile(path);
		return grIfile;
	}

	@Override
	public void setName(String name) {
		this.name = name;
	}

	@Override
	public void setID(String id) {
		this.editorID = id;
	}

	@Override
	public void setTypes(List<String> types) {
		this.types = new ArrayList<>(types);
	}

	public String getFileExtension() {
		return this.fileExtension;
	}

	public void setFileExtension(String fileExtension) {
		if (fileExtension != null) {
			if (fileExtension.startsWith(".")) {
				this.fileExtension = fileExtension.substring(1);
			} else {
				this.fileExtension = fileExtension;
			}
		}
	}

	@Override
	public void setExtension(String extension) {
		this.fileExtension = extension;
	}

	@Override
	public List<String> getTypes() {
		return this.types;
	}
}
