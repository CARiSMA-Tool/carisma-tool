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
package carisma.core.analysis;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

/**
 * This class represents the configuration of an analysis that can be applied to a given model.
 * @author wenzel
 *
 */
public class Analysis {

	/**
	 * The name of the analysis.
	 */
	private String name;
	
	/**
	 * Reference to the IFile object of the model to be analyzed. 
	 */
	private transient IFile iFile = null;
	
	/**
	 * The model to which the analysis will be applied.
	 */
	private File modelFile;
	
	/**
	 * The model type.
	 */
	private String modelType;
	
	/**
	 * The checks executed by this analysis.
	 */
	private List<CheckReference> checks;
	
	/**
	 * Name of the selected editor to open the model with.
	 */
	private String selectedEditor = "";
	
	/**
	 * Creates a new analysis.
	 * @param name Name of the analysis
	 * @param modelFile model file of analysis
	 * @param modelType model type of analysis
	 */
	public Analysis(final String name, final String modelType, final IFile modelFile) {
		super();
		this.name = name;
		this.modelType = modelType;
		this.modelFile = modelFile.getFullPath().toFile();
		this.checks = new ArrayList<>();
		this.selectedEditor = "";
		

	}
	
	/**
	 * Returns the name of the analysis.
	 * @return the name of the analysis
	 */
	public final String getName() {
		return this.name;
	}
	
	/**
	 * Returns a file object of the file to be analyzed.
	 * @return the model file of this analysis
	 */
	public final File getModelFile() {
		return this.modelFile;
	}
	
	/**
	 * Returns the model type.
	 * @return the model type of the analysis
	 */
	public final String getModelType() {
		return this.modelType;
	}
	
	/**
	 * @return Return the IFile object of the model to be analyzed.
	 */
	public final IFile getIFile() {
		if (this.iFile == null) {
			IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
			IPath p = Path.fromOSString(this.modelFile.toString());
			this.iFile = workspaceRoot.getFile(p);
		}
		return this.iFile;
	}

	/**
	 * Set the IFile object of the model and the name of model file.
	 * @param file the IFile object of the model
	 */
	public final  void setIFile(final IFile file) {
		this.iFile = file;
		this.modelFile = file.getFullPath().toFile();
	}
	
	/**
	 * Returns the list of checks that are to be executed by this analysis.
	 * The list can be manipulated.
	 * @return a list of CheckReferences
	 */
	public final List<CheckReference> getChecks() {
		return this.checks;
	}
	
	/**
	 * Sets the name of the analysis.
	 * @param name the name of the analysi
	 */
	public final void setName(final String name) {
		this.name = name;
	}
	
	/**
	 * Sets the id of the selected editor to open the model with (@see EditorTranslator).
	 * @param id the id, which is saved in the analysis
	 */
	public final void setSelectedEditorId(final String id) {
		this.selectedEditor = id;
	}
	
	/**
	 * Returns the id of the selected editor to open a model with (@see Editor Translator).
	 * @return the Name
	 */
	public final String getSelectedEditorId() {
		return this.selectedEditor;
	}
	
	/**
	 * Returns the parameters of checks in this analysis that are required and not set.
	 * @return List<CheckReference> List of Checks with invalid parameters
	 */
	public final List<CheckReference> getChecksWithInvalidParameters() {
		List<CheckReference> checksWithInvalidParameters = new ArrayList<>();
		
		for (CheckReference check : this.checks) {
			if (!check.getUnsetRequiredParameters().isEmpty()) { 
				checksWithInvalidParameters.add(check);
			}
		}
		
		return checksWithInvalidParameters;
	}
}
