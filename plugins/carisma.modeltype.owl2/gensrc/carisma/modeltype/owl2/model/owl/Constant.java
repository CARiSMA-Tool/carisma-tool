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
 * A representation of the model object '<em><b>Constant</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Constant#getLexicalValue <em>Lexical Value</em>}</li>
 *   <li>{@link carisma.modeltype.owl2.model.owl.Constant#getDatatype <em>Datatype</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getConstant()
 * @model
 * @generated
 */
public interface Constant extends EObject {
	/**
	 * Returns the value of the '<em><b>Lexical Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Lexical Value</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Lexical Value</em>' attribute.
	 * @see #setLexicalValue(String)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getConstant_LexicalValue()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	String getLexicalValue();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Constant#getLexicalValue <em>Lexical Value</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Lexical Value</em>' attribute.
	 * @see #getLexicalValue()
	 * @generated
	 */
	void setLexicalValue(String value);

	/**
	 * Returns the value of the '<em><b>Datatype</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Datatype</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Datatype</em>' reference.
	 * @see #setDatatype(Datatype)
	 * @see carisma.modeltype.owl2.model.owl.OwlPackage#getConstant_Datatype()
	 * @model required="true" ordered="false"
	 * @generated
	 */
	Datatype getDatatype();

	/**
	 * Sets the value of the '{@link carisma.modeltype.owl2.model.owl.Constant#getDatatype <em>Datatype</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Datatype</em>' reference.
	 * @see #getDatatype()
	 * @generated
	 */
	void setDatatype(Datatype value);

} // Constant
