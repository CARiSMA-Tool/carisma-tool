/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *    {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.bpmn2.extended;


import org.eclipse.bpmn2.Definitions;
import org.eclipse.emf.ecore.EObject;

import carisma.modeltype.bpmn2.extension.ExtensionRoot;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Document Root</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtendedDefinitions <em>Extended Definitions</em>}</li>
 *   <li>{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtensionRoot <em>Extension Root</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedDocumentRoot()
 * @model
 * @generated
 */
public interface ExtendedDocumentRoot extends EObject {
	/**
	 * Returns the value of the '<em><b>Extended Definitions</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Extended Definitions</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Extended Definitions</em>' containment reference.
	 * @see #setExtendedDefinitions(Definitions)
	 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedDocumentRoot_ExtendedDefinitions()
	 * @model containment="true" required="true"
	 * @generated
	 */
	Definitions getExtendedDefinitions();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtendedDefinitions <em>Extended Definitions</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Extended Definitions</em>' containment reference.
	 * @see #getExtendedDefinitions()
	 * @generated
	 */
	void setExtendedDefinitions(Definitions value);

	/**
	 * Returns the value of the '<em><b>Extension Root</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Extension Root</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Extension Root</em>' containment reference.
	 * @see #setExtensionRoot(ExtensionRoot)
	 * @see carisma.modeltype.bpmn2.extended.ExtendedPackage#getExtendedDocumentRoot_ExtensionRoot()
	 * @model containment="true"
	 * @generated
	 */
	ExtensionRoot getExtensionRoot();

	/**
	 * Sets the value of the '{@link carisma.modeltype.bpmn2.extended.ExtendedDocumentRoot#getExtensionRoot <em>Extension Root</em>}' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Extension Root</em>' containment reference.
	 * @see #getExtensionRoot()
	 * @generated
	 */
	void setExtensionRoot(ExtensionRoot value);

} // ExtendedDocumentRoot
