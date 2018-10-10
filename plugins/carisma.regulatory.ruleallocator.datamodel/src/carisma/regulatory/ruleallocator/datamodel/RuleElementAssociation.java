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
 * A representation of the model object '<em><b>Rule Element Assosation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getSrc1 <em>Src1</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getTarget1 <em>Target1</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getType1 <em>Type1</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosation()
 * @model
 * @generated
 */
public interface RuleElementAssociation extends EObject {
	/**
	 * Returns the value of the '<em><b>Src1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Src1</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Src1</em>' reference.
	 * @see #setSrc1(RuleElement)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosation_Src1()
	 * @model required="true"
	 * @generated
	 */
	RuleElement getSrc1();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getSrc1 <em>Src1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Src1</em>' reference.
	 * @see #getSrc1()
	 * @generated
	 */
	void setSrc1(RuleElement value);

	/**
	 * Returns the value of the '<em><b>Target1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Target1</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Target1</em>' reference.
	 * @see #setTarget1(RuleElement)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosation_Target1()
	 * @model required="true"
	 * @generated
	 */
	RuleElement getTarget1();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getTarget1 <em>Target1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Target1</em>' reference.
	 * @see #getTarget1()
	 * @generated
	 */
	void setTarget1(RuleElement value);

	/**
	 * Returns the value of the '<em><b>Type1</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type1</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type1</em>' reference.
	 * @see #setType1(RuleElementAssociationType)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElementAssosation_Type1()
	 * @model required="true"
	 * @generated
	 */
	RuleElementAssociationType getType1();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation#getType1 <em>Type1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type1</em>' reference.
	 * @see #getType1()
	 * @generated
	 */
	void setType1(RuleElementAssociationType value);

} // RuleElementAssosation
