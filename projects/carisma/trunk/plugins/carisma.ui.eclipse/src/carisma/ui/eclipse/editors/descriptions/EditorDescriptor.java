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

/**
 * EditorDescriptor interface, e.g. implemented by AbstractEditorDescriptor.
 */
public interface EditorDescriptor {

	/**
	 * Getter for the EditorDescriptor name.
	 * @return the name
	 */
	String getName();

	/**
	 * Getter for the EditorDescriptor id.
	 * @return the Id
	 */
	String getId();

	/**
	 * If EditorDescriptor is available in the registry. 
	 * @return boolean
	 */
	boolean isAvailable();

	/**
	 * Check if EditorDescriptor is applicable for <code>file</code>.
	 * @param modelIFile the model IFile
	 * @return boolean 
	 */
	boolean isApplicable(IFile modelIFile);

	/**
	 * Opens the IFile.
	 * @param file the file
	 * @return boolean
	 */
	boolean forceOpen(IFile file);
	
	/** Set the name of the Editor, used in the ui.
	 * 
	 * @param name Name of the Editor.
	 */
	void setName(String name);
	
	/** Set the id of Editor with which is registered in eclipse.
	 * 
	 * @param id the id.
	 */
	void setID(String id);
	
	/** Sets a List of file extensions which can be opened with this editor.
	 * 
	 * @param types the list of types.
	 */
	void setTypes(List<String> types);
	
	/**
	 * Getter for the list of file extensions which can be opened with this editor.
	 * 
	 * @return list of supported types.
	 */
	List<String> getTypes();
	
	/** Sets the optional setting if the file to be opened differs from the model type.
	 * 
	 * @param extension The extension.
	 */
	void setExtension(String extension);
}
