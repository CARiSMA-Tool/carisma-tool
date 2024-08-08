/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Constraint</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getLeftOperand <em>Left Operand</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getStatus <em>Status</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getOperator <em>Operator</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getRightOperand <em>Right Operand</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getRightOperandReference <em>Right Operand Reference</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getDataType <em>Data Type</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.Constraint#getUnit <em>Unit</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint()
 * @model
 * @generated
 */
public interface Constraint extends EObject {
	/**
	 * Returns the value of the '<em><b>Uid</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Uid</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Uid</em>' attribute.
	 * @see #setUid(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_Uid()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getUid();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getUid <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uid</em>' attribute.
	 * @see #getUid()
	 * @generated
	 */
	void setUid(String value);

	/**
	 * Returns the value of the '<em><b>Left Operand</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.LeftOperand}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Left Operand</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Left Operand</em>' attribute.
	 * @see ODRLCommonVocabulary.LeftOperand
	 * @see #setLeftOperand(LeftOperand)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_LeftOperand()
	 * @model default="Null" required="true" ordered="false"
	 * @generated
	 */
	LeftOperand getLeftOperand();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getLeftOperand <em>Left Operand</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Left Operand</em>' attribute.
	 * @see ODRLCommonVocabulary.LeftOperand
	 * @see #getLeftOperand()
	 * @generated
	 */
	void setLeftOperand(LeftOperand value);

	/**
	 * Returns the value of the '<em><b>Status</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Status</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Status</em>' attribute.
	 * @see #setStatus(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_Status()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getStatus();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getStatus <em>Status</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Status</em>' attribute.
	 * @see #getStatus()
	 * @generated
	 */
	void setStatus(String value);

	/**
	 * Returns the value of the '<em><b>Operator</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.ConstraintOperator}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Operator</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Operator</em>' attribute.
	 * @see ODRLCommonVocabulary.ConstraintOperator
	 * @see #setOperator(ConstraintOperator)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_Operator()
	 * @model default="Null" required="true" ordered="false"
	 * @generated
	 */
	ConstraintOperator getOperator();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getOperator <em>Operator</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Operator</em>' attribute.
	 * @see ODRLCommonVocabulary.ConstraintOperator
	 * @see #getOperator()
	 * @generated
	 */
	void setOperator(ConstraintOperator value);

	/**
	 * Returns the value of the '<em><b>Right Operand</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right Operand</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right Operand</em>' attribute list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_RightOperand()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getRightOperand();

	/**
	 * Returns the value of the '<em><b>Right Operand Reference</b></em>' attribute list.
	 * The list contents are of type {@link java.lang.String}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Right Operand Reference</em>' attribute list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Right Operand Reference</em>' attribute list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_RightOperandReference()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	EList<String> getRightOperandReference();

	/**
	 * Returns the value of the '<em><b>Data Type</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Data Type</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Data Type</em>' attribute.
	 * @see #setDataType(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_DataType()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getDataType();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getDataType <em>Data Type</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Data Type</em>' attribute.
	 * @see #getDataType()
	 * @generated
	 */
	void setDataType(String value);

	/**
	 * Returns the value of the '<em><b>Unit</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Unit</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Unit</em>' attribute.
	 * @see #setUnit(String)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getConstraint_Unit()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getUnit();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.Constraint#getUnit <em>Unit</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Unit</em>' attribute.
	 * @see #getUnit()
	 * @generated
	 */
	void setUnit(String value);

} // Constraint
