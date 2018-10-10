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

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Annotation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Annotation#getAnnotationProperty <em>Annotation Property</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAnnotation()
 * @model
 * @generated
 */
public interface Annotation extends EObject {
	/**
	 * Returns the value of the '<em><b>Annotation Property</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Annotation Property</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Annotation Property</em>' reference.
	 * @see #setAnnotationProperty(AnnotationProperty)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAnnotation_AnnotationProperty()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	AnnotationProperty getAnnotationProperty();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Annotation#getAnnotationProperty <em>Annotation Property</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Annotation Property</em>' reference.
	 * @see #getAnnotationProperty()
	 * @generated
	 */
	void setAnnotationProperty(AnnotationProperty value);

} // Annotation
