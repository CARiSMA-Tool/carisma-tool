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

import org.eclipse.emf.common.util.EList;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Anonymous Individual Annotation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndividual <em>Anonymous Individual</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndiviudalAnnotations <em>Anonymous Indiviudal Annotations</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAnonymousIndividualAnnotation()
 * @model
 * @generated
 */
public interface AnonymousIndividualAnnotation extends Axiom {
	/**
	 * Returns the value of the '<em><b>Anonymous Individual</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Anonymous Individual</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Anonymous Individual</em>' reference.
	 * @see #setAnonymousIndividual(AnonymousIndividual)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAnonymousIndividualAnnotation_AnonymousIndividual()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	AnonymousIndividual getAnonymousIndividual();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.AnonymousIndividualAnnotation#getAnonymousIndividual <em>Anonymous Individual</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Anonymous Individual</em>' reference.
	 * @see #getAnonymousIndividual()
	 * @generated
	 */
	void setAnonymousIndividual(AnonymousIndividual value);

	/**
	 * Returns the value of the '<em><b>Anonymous Indiviudal Annotations</b></em>' reference list.
	 * The list contents are of type {@link carisma.modeltype.owl2.model.owl.Annotation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Anonymous Indiviudal Annotations</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Anonymous Indiviudal Annotations</em>' reference list.
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getAnonymousIndividualAnnotation_AnonymousIndiviudalAnnotations()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	EList<Annotation> getAnonymousIndiviudalAnnotations();

} // AnonymousIndividualAnnotation
