/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Rule Element Assosiation Type</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getSrc <em>Src</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getTarget <em>Target</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosiationType()
 * @model
 * @generated
 */
public interface RuleElementAssociationType extends EObject {
	/**
	 * Returns the value of the '<em><b>Src</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Src</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Src</em>' reference.
	 * @see #setSrc(RuleElementType)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosiationType_Src()
	 * @model required="true"
	 * @generated
	 */
	RuleElementType getSrc();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getSrc <em>Src</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Src</em>' reference.
	 * @see #getSrc()
	 * @generated
	 */
	void setSrc(RuleElementType value);

	/**
	 * Returns the value of the '<em><b>Target</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Target</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Target</em>' reference.
	 * @see #setTarget(RuleElementType)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosiationType_Target()
	 * @model required="true"
	 * @generated
	 */
	RuleElementType getTarget();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getTarget <em>Target</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Target</em>' reference.
	 * @see #getTarget()
	 * @generated
	 */
	void setTarget(RuleElementType value);

	/**
	 * Returns the value of the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Name</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Name</em>' attribute.
	 * @see #setName(String)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosiationType_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

} // RuleElementAssosiationType
