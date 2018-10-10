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

import carisma.ui.eclipse.editors.AbstractEditorDescriptor;


/**Replaced with carisma.ui.eclipse.descriptor 
 * 
 * Extends the AbstractEditorDescriptor for an TextEditor.
 */
@Deprecated
public class TextEditorDescriptor extends AbstractEditorDescriptor {

	/**
	 * EditorDescriptor name.
	 */
	public static final String NAME = "Text Editor";
	
	//########################################################################################
	/**
	 * Constructor.
	 */
	public TextEditorDescriptor() {
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
		return "org.eclipse.ui.DefaultTextEditor";
	}

	/**
	 * Check if EditorDescriptor is applicable for <code>file</code>.
	 * @param modelIFile the model IFile
	 * @return boolean
	 */
	@Override
	public final boolean isApplicable(final IFile modelIFile) {
		return true;
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
