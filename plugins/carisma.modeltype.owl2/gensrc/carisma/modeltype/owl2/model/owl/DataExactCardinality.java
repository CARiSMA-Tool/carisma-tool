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

import java.util.Map;

import org.eclipse.emf.common.util.DiagnosticChain;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Data Exact Cardinality</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getCardinality <em>Cardinality</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataRange <em>Data Range</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataPropertyExpression <em>Data Property Expression</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataExactCardinality()
 * @model
 * @generated
 */
public interface DataExactCardinality extends ClassExpression {
	/**
	 * Returns the value of the '<em><b>Cardinality</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Cardinality</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Cardinality</em>' attribute.
	 * @see #setCardinality(int)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataExactCardinality_Cardinality()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	int getCardinality();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getCardinality <em>Cardinality</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Cardinality</em>' attribute.
	 * @see #getCardinality()
	 * @generated
	 */
	void setCardinality(int value);

	/**
	 * Returns the value of the '<em><b>Data Range</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Range</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Range</em>' reference.
	 * @see #setDataRange(DataRange)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataExactCardinality_DataRange()
	 * @model ordered="false"
	 * @generated
	 */
	DataRange getDataRange();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataRange <em>Data Range</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Range</em>' reference.
	 * @see #getDataRange()
	 * @generated
	 */
	void setDataRange(DataRange value);

	/**
	 * Returns the value of the '<em><b>Data Property Expression</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Property Expression</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Property Expression</em>' reference.
	 * @see #setDataPropertyExpression(DataPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataExactCardinality_DataPropertyExpression()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	DataPropertyExpression getDataPropertyExpression();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataExactCardinality#getDataPropertyExpression <em>Data Property Expression</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Property Expression</em>' reference.
	 * @see #getDataPropertyExpression()
	 * @generated
	 */
	void setDataPropertyExpression(DataPropertyExpression value);

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * <!-- begin-model-doc -->
	 * self.cardinality>=0
	 * <!-- end-model-doc -->
	 * @model
	 * @generated
	 */
	boolean Thecardinalitymustbenonnegative(DiagnosticChain diagnostics, Map context);

} // DataExactCardinality
