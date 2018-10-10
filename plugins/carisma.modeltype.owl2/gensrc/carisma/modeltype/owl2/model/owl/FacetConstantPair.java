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
 * A representation of the model object '<em><b>Facet Constant Pair</b></em>'.
 * <!-- end-user-doc -->
 *
 * <!-- begin-model-doc -->
 * self.facet = 'length' 
 * or self.facet = 'minLength' 
 * or self.facet = 'maxLength' 
 * or self.facet = 'pattern' 
 * or self.facet = 'minInclusive' 
 * or self.facet = 'minExclusive' 
 * or self.facet = 'maxInclusive'
 * or self.facet = 'maxExclusive'
 * or self.facet = 'totalDigits' 
 * or self.facet = 'fractionDigits'
 * 
 * <!-- end-model-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getConstant <em>Constant</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getFacet <em>Facet</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getFacetConstantPair()
 * @model
 * @generated
 */
public interface FacetConstantPair extends EObject {
	/**
	 * Returns the value of the '<em><b>Constant</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Constant</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Constant</em>' reference.
	 * @see #setConstant(Constant)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getFacetConstantPair_Constant()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Constant getConstant();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getConstant <em>Constant</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Constant</em>' reference.
	 * @see #getConstant()
	 * @generated
	 */
	void setConstant(Constant value);

	/**
	 * Returns the value of the '<em><b>Facet</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Facet</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Facet</em>' attribute.
	 * @see #setFacet(String)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getFacetConstantPair_Facet()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	String getFacet();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.FacetConstantPair#getFacet <em>Facet</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Facet</em>' attribute.
	 * @see #getFacet()
	 * @generated
	 */
	void setFacet(String value);

} // FacetConstantPair
