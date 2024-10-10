/**
 */
package ODRLCommonVocabulary;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Logical Constraint</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 *   <li>{@link ODRLCommonVocabulary.LogicalConstraint#getUid <em>Uid</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.LogicalConstraint#getLogicalOperator <em>Logical Operator</em>}</li>
 *   <li>{@link ODRLCommonVocabulary.LogicalConstraint#getConstraints <em>Constraints</em>}</li>
 * </ul>
 *
 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getLogicalConstraint()
 * @model
 * @generated
 */
public interface LogicalConstraint extends EObject {
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
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getLogicalConstraint_Uid()
	 * @model dataType="org.eclipse.uml2.types.String" ordered="false"
	 * @generated
	 */
	String getUid();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.LogicalConstraint#getUid <em>Uid</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Uid</em>' attribute.
	 * @see #getUid()
	 * @generated
	 */
	void setUid(String value);

	/**
	 * Returns the value of the '<em><b>Logical Operator</b></em>' attribute.
	 * The default value is <code>"Null"</code>.
	 * The literals are from the enumeration {@link ODRLCommonVocabulary.LogicalOperator}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Logical Operator</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Logical Operator</em>' attribute.
	 * @see ODRLCommonVocabulary.LogicalOperator
	 * @see #setLogicalOperator(LogicalOperator)
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getLogicalConstraint_LogicalOperator()
	 * @model default="Null" ordered="false"
	 * @generated
	 */
	LogicalOperator getLogicalOperator();

	/**
	 * Sets the value of the '{@link ODRLCommonVocabulary.LogicalConstraint#getLogicalOperator <em>Logical Operator</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Logical Operator</em>' attribute.
	 * @see ODRLCommonVocabulary.LogicalOperator
	 * @see #getLogicalOperator()
	 * @generated
	 */
	void setLogicalOperator(LogicalOperator value);

	/**
	 * Returns the value of the '<em><b>Constraints</b></em>' containment reference list.
	 * The list contents are of type {@link ODRLCommonVocabulary.Constraint}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Constraints</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Constraints</em>' containment reference list.
	 * @see ODRLCommonVocabulary.ODRLCommonVocabularyPackage#getLogicalConstraint_Constraints()
	 * @model containment="true" required="true"
	 * @generated
	 */
	EList<Constraint> getConstraints();

} // LogicalConstraint
