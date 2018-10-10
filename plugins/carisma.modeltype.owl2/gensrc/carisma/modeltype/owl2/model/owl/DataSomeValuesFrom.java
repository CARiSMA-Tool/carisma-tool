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
 * A representation of the model object '<em><b>Data Some Values From</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataRange <em>Data Range</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataPropertyExpressions <em>Data Property Expressions</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataSomeValuesFrom()
 * @model
 * @generated
 */
public interface DataSomeValuesFrom extends ClassExpression {
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
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataSomeValuesFrom_DataRange()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	DataRange getDataRange();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataRange <em>Data Range</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Range</em>' reference.
	 * @see #getDataRange()
	 * @generated
	 */
	void setDataRange(DataRange value);

	/**
	 * Returns the value of the '<em><b>Data Property Expressions</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Property Expressions</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Property Expressions</em>' reference.
	 * @see #setDataPropertyExpressions(DataPropertyExpression)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getDataSomeValuesFrom_DataPropertyExpressions()
	 * @model required="true"
	 * @generated
	 */
	DataPropertyExpression getDataPropertyExpressions();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.DataSomeValuesFrom#getDataPropertyExpressions <em>Data Property Expressions</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Property Expressions</em>' reference.
	 * @see #getDataPropertyExpressions()
	 * @generated
	 */
	void setDataPropertyExpressions(DataPropertyExpression value);

} // DataSomeValuesFrom
