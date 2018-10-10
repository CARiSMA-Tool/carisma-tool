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
package carisma.ui.eclipse.preferences;

/**
 * Constant class for Preferences.
 *
 */
public final class Constants {
	
	/**
	 * Hide constructor.
	 */
	private Constants() {
	}

	/**
	 * Identifier for PrefAnalyse in Preferences.
	 */
	public static final String PREF_ANALYSE = "boolean analyse all";

	/**
	 * Id of the perspective.
	 */
	public static final String PERSPECTIVE_ID = "perspective_id";

	/**
	 * id of the editor.
	 */
	public static final String EDITOR_ID = "editor_id";
	
	
	/**
	 * identifier for the selection of the editor.
	 */
	public static final String EDITOR_SELECTION_ART = "editor_selection_art";
	
	/**
	 * Text editor Id.
	 */
	public static final String TEXT_EDITOR_ID = "org.eclipse.ui.DefaultTextEditor"; 
	/**
	 * UML Editor Id.
	 */
	public static final String UML_EDITOR_ID = "org.eclipse.uml2.uml.editor.presentation.UMLEditorID";
	/**
	 * Papyrus Editor Id.
	 */
	public static final String PAPYRUS_ID = "org.eclipse.papyrus.core.papyrusEditor";
	/**
	 * Topcased Editor Id.
	 */
	public static final String TOPCASED_ID = "org.topcased.modeler.uml.editor.UMLEditor";
	/**
	 * BPMN2 Modeler Id. (Visual Editor)
	 */
	public static final String BPMN2_MODELER_EDITOR_ID = "org.eclipse.bpmn2.modeler.ui.bpmn2editor";
	/**
	 * BPMN2 Editor Id.
	 */
	public static final String BPMN2_EDITOR_ID = "org.eclipse.bpmn2.presentation.Bpmn2EditorID";
	/**
	 * Ecore Editor Id.
	 */
	public static final String ECORE_EDITOR_ID = "org.eclipse.emf.ecore.presentation.EcoreEditorID";
	
	
	/**
	 * Consts, used for the selection Art.
	 */
	//public static final String DEFAULT = "default";
	/**
	 * Constant for Manually. 
	 */
	public static final String MANUALLY = "manually";
	/**
	 * Constant for Auto.
	 */
	public static final String AUTO = "auto";
	/**
	 * Identifier for Editor List in Preferences.
	 */
	public static final String EDITORS_LIST = "editors_list";
	/**
	 * Constant for Manually Choice.
	 */
	public static final String CHOOSE_MANUALLY = "Choose manually";
}
