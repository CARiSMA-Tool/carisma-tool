/**
 * Copyright (c) 2011 Software Engineering Institute, TU Dortmund.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     {SecSE group} - initial API and implementation and/or initial documentation
 */
package carisma.modeltype.owl2.model.owl;


/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Inverse Object Property</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.InverseObjectProperty#getObjectProperty <em>Object Property</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getInverseObjectProperty()
 * @model
 * @generated
 */
public interface InverseObjectProperty extends ObjectPropertyExpression {
	/**
	 * Returns the value of the '<em><b>Object Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Object Property</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Object Property</em>' reference.
	 * @see #setObjectProperty(ObjectProperty)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getInverseObjectProperty_ObjectProperty()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	ObjectProperty getObjectProperty();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.InverseObjectProperty#getObjectProperty <em>Object Property</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Object Property</em>' reference.
	 * @see #getObjectProperty()
	 * @generated
	 */
	void setObjectProperty(ObjectProperty value);

} // InverseObjectProperty
