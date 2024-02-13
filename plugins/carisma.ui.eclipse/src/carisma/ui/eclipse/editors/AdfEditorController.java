package carisma.ui.eclipse.editors;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;

import carisma.core.analysis.Analysis;
import carisma.core.analysis.BooleanParameter;
import carisma.core.analysis.CheckReference;
import carisma.core.analysis.FloatParameter;
import carisma.core.analysis.FolderParameter;
import carisma.core.analysis.InputFileParameter;
import carisma.core.analysis.IntegerParameter;
import carisma.core.analysis.OutputFileParameter;
import carisma.core.analysis.StringParameter;
import carisma.core.checks.CheckDescriptor;
import carisma.core.checks.CheckParameter;
import carisma.core.checks.CheckRegistry;
import carisma.core.checks.ParameterType;
import carisma.ui.eclipse.CarismaGUI;

/**
 * Model = Analysis. View = AdfEditor. Controller = AdfEditorController.
 * 
 * @author Johannes Kowald
 * 
 */
public class AdfEditorController {

	/**
	 * A static string for the source of problems regarding to the list of
	 * checks.
	 */
	protected static final String PROBLEM_CHECK = "List of checks";

	/**
	 * A static string for the source of problems regarding to the model.
	 */
	protected static final String PROBLEM_MODEL = "Model";

	/**
	 * Corresponding {@link Analysis}.
	 */
	private Analysis analysis;

	/**
	 * Corresponding {@link AdfEditor}.
	 */
	private AdfEditor editor;

	/**
	 * The list of problems.
	 */
	private Map<Object, Object> problemList;

	/**
	 * Constructor.
	 * 
	 * @param editor
	 *            Corresponding {@link AdfEditor}
	 * @param analysis
	 *            Corresponding {@link Analysis}
	 */
	public AdfEditorController(final AdfEditor editor, final Analysis analysis) {
		this.editor = editor;
		this.analysis = analysis;
		this.problemList = new HashMap<>();
	}

	/**
	 * Indicates if the {@link AdfEditor} is in a dirty state.
	 * 
	 * @return The dirty state of the corresponding {@link AdfEditor}
	 */
	protected final boolean isEditorDirty() {
		return this.editor.isDirty();
	}

	/**
	 * Provides the list of {@link CheckReference}s to the GUI.
	 * 
	 * @return a list of {@link CheckReference}s as input to the check list
	 *         viewer.
	 */
	protected final List<CheckReference> getSelectedChecksList() {
		return this.analysis.getChecks();
	}

	/**
	 * Creates a new {@link CheckReference} and adds it to the list of
	 * {@link CheckReference}s in the corresponding {@link Analysis}.
	 * 
	 * @param checkDescriptorId
	 *            The ID of the {@link CheckDescriptor} which will be created
	 */
	protected final void createCheck(final String checkDescriptorId) {
		CarismaGUI.getCheckRegistry();
		CheckReference reference = CheckRegistry
				.createReference(
						CarismaGUI.getCheckRegistry()
								.getCheckDescriptor(checkDescriptorId));
		addCheck(reference);
	}

	/**
	 * Adds a {@link CheckReference} to the list of {@link CheckReference}s in
	 * the corresponding {@link Analysis}.
	 * 
	 * @param checkReference
	 *            The {@link CheckReference} which should be added
	 */
	protected final void addCheck(final CheckReference checkReference) {
		this.analysis.getChecks().add(checkReference);
		this.editor.setDirty(true);
	}

	/**
	 * Removes a {@link CheckReference} from the list of {@link CheckReference}s
	 * in the corresponding {@link Analysis}.
	 * 
	 * @param checkReference
	 *            The {@link CheckReference} which should be removed
	 */
	protected final void removeCheck(final CheckReference checkReference) {
		this.analysis.getChecks().remove(checkReference);
		this.editor.setDirty(true);
	}

	/**
	 * Provides the name of the {@link Analysis}.
	 * 
	 * @return the name of the {@link Analysis}
	 */
	protected final String getAnalysisName() {
		return this.analysis.getName();
	}

	/**
	 * Sets the name of the {@link Analysis} and tells the {@link AdfEditor} to
	 * be dirty.
	 * 
	 * @param name
	 *            the new name of the {@link Analysis}
	 */
	protected final void setAnalysisName(final String name) {
		if (!this.analysis.getName().equals(name)) {
			this.analysis.setName(name);
			this.editor.setDirty(true);
		}
	}

	/**
	 * Provides the model type of the {@link Analysis}.
	 * 
	 * @return the model type of the {@link Analysis} as a String
	 */
	protected final String getModelType() {
		return this.analysis.getModelType();
	}

	/**
	 * Provides the model {@link IFile} of the analysis.
	 * 
	 * @return The model file of the {@link Analysis} as an {@link IFile}
	 */
	protected final IFile getModelIFile() {
		return this.analysis.getIFile();
	}

	/**
	 * Sets the model {@link IFile} of the the {@link Analysis}.
	 * 
	 * @param newIFile
	 *            The new model {@link IFile}
	 */
	protected final void setModelIFile(final IFile newIFile) {
		if (!this.analysis.getIFile().equals(newIFile)) {
			this.analysis.setIFile(newIFile);
			this.editor.setDirty(true);
		}
	}

	/**
	 * Indicates if the {@link Analysis}' model file is valid.
	 * 
	 * @return true if the {@link Analysis}' model file is valid
	 */
	protected final boolean isModelFileValid() {
		return this.analysis.getIFile().exists();
		// Probably proof more details then only the existence of the file
	}

	/**
	 * Provides the {@link Analysis}' selected editor id.
	 * 
	 * @return the id of the selected editor of the {@link Analysis}
	 */
	protected final String getSelectedEditorId() {
		return this.analysis.getSelectedEditorId();
	}

	/**
	 * Sets the {@link Analysis}' selected editor id.
	 * 
	 * @param newEditorId
	 *            the new editor id
	 */
	protected final void setSelectedEditorId(final String newEditorId) {
		if (!this.analysis.getSelectedEditorId().equals(newEditorId)) {
			this.analysis.setSelectedEditorId(newEditorId);
			this.editor.setDirty(true);
		}
	}

	/**
	 * Enables/Disables a {@link CheckReference} and tells the {@link AdfEditor}
	 * to be dirty if necessary.
	 * 
	 * @param checkReference
	 *            the {@link CheckReference} to enable/disable
	 * @param enabled
	 *            the new enable state
	 */
	protected final void setCheckSelection(final CheckReference checkReference,
			final boolean enabled) {
		if (checkReference.isEnabled() != enabled) {
			checkReference.setEnabled(enabled);
			this.editor.setDirty(true);
		}
	}

	/**
	 * Loads the corresponding {@link Analysis}.
	 */
	protected final void loadAnalysis() {
		this.analysis = this.editor.loadAnalysis();
	}

	/**
	 * Saves the corresponding {@link Analysis}.
	 */
	protected final void saveAnalysis() {
		this.editor.saveAnalysis();
	}

	/**
	 * Opens the correct editor for the model file regarding to the choices made
	 * in CARiSMA preferences and the editor combo box.
	 * 
	 * @param defaultEditor
	 *            indicates if the default editor should be opened
	 */
	protected final void openModelEditor(final boolean defaultEditor) {
		EditorTranslator editorTranslator = new EditorTranslator(this.analysis);
		editorTranslator.openEditor(defaultEditor);
	}

	/**
	 * Runs the corresponding {@link Analysis}.
	 */
	protected final void runAnalysis() {
		if (!this.editor.isDirty()) {
			CarismaGUI.runAnalysis(this.analysis);
		}
	}

	/**
	 * Returns the {@link IFile} of the selected model file and creates, if
	 * necessary, a link to a non workspace file.
	 * 
	 * @param filepath
	 *            Path to the model file
	 * @return The {@link IFile} object of the selected file
	 * @throws CoreException
	 *             If the {@link IFile} could not be created
	 */
	protected final static IFile getLinkedIFile(final String filepath)
			throws CoreException {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IPath location = Path.fromOSString(filepath);
		IFile modelIFile = workspace.getRoot().getFileForLocation(location);

		if (modelIFile != null) {
			// If the file is in scope of the workspace
			return modelIFile;
		}
		// If the file is outside the scope of the workspace
		IProject projectExternal = workspace.getRoot().getProject(
				"External Files");
		if (!projectExternal.exists()) {
			projectExternal.create(null);
		}
		if (!projectExternal.isOpen()) {
			projectExternal.open(null);
		}
		modelIFile = projectExternal.getFile(location.lastSegment());
		if (!modelIFile.exists()) {
			modelIFile.createLink(location, IResource.NONE, null);
			projectExternal.refreshLocal(IResource.DEPTH_INFINITE,
					new NullProgressMonitor());
		}
		return modelIFile;
	}

	/**
	 * Sets the value of a {@link CheckParameter} and tells the
	 * {@link AdfEditor} to be dirty if necessary.
	 * 
	 * @param checkParameter
	 *            the {@link CheckParameter} which will be updated
	 * @param value
	 *            the new value of the {@link CheckParameter}
	 */
	protected final void setParameter(final CheckParameter checkParameter,
			final Object value) {
		boolean newValue = false;
		if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.STRING)) {
			StringParameter stringParameter = (StringParameter) checkParameter;
			if (stringParameter.getValue() != null) {
				newValue = !stringParameter.getValue().equals(value);
			} else {
				newValue = true;
			}
			stringParameter.setValue((String) value);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.INTEGER)) {
			IntegerParameter integerParameter = (IntegerParameter) checkParameter;
			int intValue = ((Integer) value).intValue();
			newValue = !(integerParameter.getValue() == intValue);
			integerParameter.setValue(intValue);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.FLOAT)) {
			FloatParameter floatParameter = (FloatParameter) checkParameter;
			float floatValue = ((Float) value).floatValue();
			newValue = !(floatParameter.getValue() == floatValue);
			floatParameter.setValue(floatValue);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.BOOLEAN)) {
			BooleanParameter booleanParameter = (BooleanParameter) checkParameter;
			boolean booleanValue = ((Boolean) value).booleanValue();
			newValue = !(booleanParameter.getValue() == booleanValue);
			booleanParameter.setValue(booleanValue);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.INPUTFILE)) {
			InputFileParameter inputFileParameter = (InputFileParameter) checkParameter;
			if (value != null && inputFileParameter.getValue() != null) {
				newValue = !inputFileParameter.getValue().equals(value);
			} else {
				newValue = true;
			}
			inputFileParameter.setValue((File) value);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.FOLDER)) {
			FolderParameter folderParameter = (FolderParameter) checkParameter;
			if (value != null && folderParameter.getValue() != null) {
				newValue = !folderParameter.getValue().equals(value);
			} else {
				newValue = true;
			}
			folderParameter.setValue((File) value);
		} else if (checkParameter.getDescriptor().getType()
				.equals(ParameterType.OUTPUTFILE)) {
			OutputFileParameter outputFileParameter = (OutputFileParameter) checkParameter;
			if (value != null && outputFileParameter.getValue() != null) {
				newValue = !outputFileParameter.getValue().equals(value);
			} else {
				newValue = true;
			}
			outputFileParameter.setValue((File) value);
		}
		if (newValue) {
			this.editor.setDirty(true);
		}
	}

	/**
	 * Sets the query on demand flag of a {@link CheckParameter} and tells the
	 * {@link AdfEditor} to be dirty if necessary.
	 * 
	 * @param checkParameter
	 *            the {@link CheckParameter} which will be updated
	 * @param qod
	 *            query on demand flag
	 */
	protected final void setParameterQod(final CheckParameter checkParameter,
			final boolean qod) {
		if (checkParameter.isQueryOnDemand() != qod) {
			checkParameter.setQueryOnDemand(qod);
			this.editor.setDirty(true);
		}
	}

	/**
	 * Iterates over all checks to proof, that the not optional parameters have
	 * valid values or set to query on demand. If a check is disabled, it will
	 * be ignored. If a parameter is optional, it will be ignored.
	 * 
	 * @return True if there are invalid parameters, false otherwise
	 */
	protected final boolean hasAnalysisInvalidParameters() {
		boolean valid = true;
		for (CheckReference check : this.analysis.getChecks()) {
			if (!check.isEnabled()) {
				removeProblem(check, null);
				continue;
			}
			for (CheckParameter parameter : check.getParameters()) {
				// NULL POINTER SAFETY
				if (parameter == null) {
					addProblem(check.getCheckID(), " A parameter is null");
					valid = false;
					continue;
				}
				if (parameter.isQueryOnDemand()
						|| parameter.getDescriptor().isOptional()) {
					removeProblem(check, parameter);
					continue;
				}
				ParameterType actualParameterType = parameter
						.getDescriptor().getType();
				// INTEGER, FLOAT AND BOOLEAN PARAMETER ARE ALWAYS VALID
				if (actualParameterType.equals(ParameterType.INTEGER)
						|| actualParameterType.equals(ParameterType.FLOAT)
						|| actualParameterType.equals(ParameterType.BOOLEAN)) {
					continue;
				}
				// STRING PARAMETER
				if (actualParameterType.equals(ParameterType.STRING)) {
					StringParameter stringParameter = (StringParameter) parameter;
					if (stringParameter.getValue() == null) {
						addProblem(check, parameter,
								"String is null, not optional and has no query on demand");
						valid = false;
						continue;
					}
					if (stringParameter.getValue().isEmpty()) {
						addProblem(check, parameter,
								"String is empty, not optional and has no query on demand");
						valid = false;
						continue;
					}
					removeProblem(check, parameter);
					continue;
				}
				// INPUT FILE
				if (actualParameterType.equals(ParameterType.INPUTFILE)) {
					InputFileParameter inputfileParameter = (InputFileParameter) parameter;
					if (inputfileParameter.getValue() == null) {
						addProblem(check, parameter,
								"File is null, not optional and has no query on demand");
						valid = false;
						continue;
					}
					if (!inputfileParameter.getValue().exists()) {
						addProblem(check, parameter,
								"File does not exist, is not optional and has no query on demand");
						valid = false;
						continue;
					}
					removeProblem(check, parameter);
					continue;
				}
				// OUTPUT FILE
				if (actualParameterType.equals(ParameterType.OUTPUTFILE)) {
					OutputFileParameter outputfileParameter = (OutputFileParameter) parameter;
					if (outputfileParameter.getValue() == null) {
						addProblem(check, parameter,
								"File is null, not optional and has no query on demand");
						valid = false;
						continue;
					}
					if (!outputfileParameter.isInsertedValueValid()) {
						addProblem(check, parameter,
								"Inserted value is not valid, not optional and has no query on demand");
						valid = false;
						continue;
					}
					removeProblem(check, parameter);
					continue;
				}
				// FOLDER PARAMETER
				if (actualParameterType.equals(ParameterType.FOLDER)) {
					FolderParameter folderParameter = (FolderParameter) parameter;
					if (folderParameter.getValue() == null) {
						addProblem(check, parameter,
								"Value is null, not optional and has no query on demand");
						valid = false;
						continue;
					}
					if (folderParameter.getValue().getAbsolutePath()
							.isEmpty()) {
						addProblem(check, parameter,
								"Absolute path is empty, parameter is not optional and has no query on demand");
						valid = false;
						continue;
					}
					removeProblem(check, parameter);
					continue;
				}
			}
		}
		return !valid;
	}

	/**
	 * Adds a problem associated to a parameter to the list.
	 * 
	 * @param checkReference
	 *            The {@link CheckReference} which has the problem
	 * @param checkParameter
	 *            The {@link CheckParameter} which has the problem
	 * @param text
	 *            The text which gives information about the problem
	 */
	protected final void addProblem(final CheckReference checkReference,
			final CheckParameter checkParameter, final String text) {
		if (this.problemList.containsKey(checkReference)) {
			Object valueObject = this.problemList.get(checkReference);
			@SuppressWarnings("unchecked")
			HashMap<CheckParameter, String> valueMap = (HashMap<CheckParameter, String>) valueObject;
			valueMap.put(checkParameter, text);
		} else {
			Map<CheckParameter, String> valueMap = new HashMap<>();
			valueMap.put(checkParameter, text);
			this.problemList.put(checkReference, valueMap);
		}
	}

	/**
	 * Adds a problem not associated to a parameter to the list.
	 * 
	 * @param source
	 *            An identifier of the source of the problem
	 * @param text
	 *            The text which gives information about the problem
	 */
	protected final void addProblem(final String source, final String text) {
		this.problemList.put(source, text);
	}

	/**
	 * Removes a problem from the list associated to a parameter.
	 * 
	 * @param checkReference
	 *            The {@link CheckReference} which lost the problem
	 * @param checkParameter
	 *            The {@link CheckParameter} which lost the problem or null if
	 *            all problems referring to the given {@link CheckReference}
	 *            should be deleted
	 */
	protected final void removeProblem(final CheckReference checkReference,
			final CheckParameter checkParameter) {
		if (this.problemList.containsKey(checkReference)) {
			if (checkParameter != null) {
				Object valueObject = this.problemList.get(checkReference);
				if (valueObject instanceof Map<?, ?>) {
					@SuppressWarnings("unchecked")
					Map<CheckParameter, String> valueMap = (Map<CheckParameter, String>) valueObject;
					valueMap.remove(checkParameter);
					if (valueMap.isEmpty()) {
						this.problemList.remove(checkReference);
					}
				}
			} else {
				this.problemList.remove(checkReference);
			}
		}
	}

	/**
	 * Removes a problem from the list not associated to a parameter.
	 * 
	 * @param source
	 *            An identifier of the source of the problem
	 */
	protected final void removeProblem(final String source) {
		this.problemList.remove(source);
	}

	/**
	 * Clears the list of problems.
	 */
	protected final void clearProblems() {
		this.problemList.clear();
	}

	/**
	 * Returns the list of problems as a list of strings.
	 * 
	 * @return A list of strings which describe the problems.
	 */
	protected final List<String> getProblems() {
		List<String> returnProblemList = new ArrayList<>();
		for (Entry<Object, Object> problemEntry : this.problemList.entrySet()) {
			if (problemEntry.getKey() instanceof String) {
				returnProblemList.add(problemEntry.getKey().toString() + ": "
						+ problemEntry.getValue().toString());
			}
			if (problemEntry.getKey() instanceof CheckReference) {
				CheckReference problemCheck = (CheckReference) problemEntry
						.getKey();
				CheckDescriptor checkDescriptor = CarismaGUI
						.getCheckRegistry().getCheckDescriptor(
								problemCheck.getCheckID());
				if (checkDescriptor != null) {
					returnProblemList.add(checkDescriptor.getName() + ":");
					if (problemEntry.getValue() instanceof Map<?, ?>) {
						@SuppressWarnings("unchecked")
						Map<CheckParameter, String> valueList = (Map<CheckParameter, String>) problemEntry
								.getValue();
						for (Entry<CheckParameter, String> problemParameterEntry : valueList
								.entrySet()) {
							returnProblemList.add("- "
									+ problemParameterEntry.getKey()
											.getDescriptor().getName() + ": "
									+ problemParameterEntry.getValue());
						}
					}
				} else {
					returnProblemList.add(problemCheck.getCheckID() + " is not a valid check ID!");
				}
			}
		}
		return returnProblemList;
	}

}
