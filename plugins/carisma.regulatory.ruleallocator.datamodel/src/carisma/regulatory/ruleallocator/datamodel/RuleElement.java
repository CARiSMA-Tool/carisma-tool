/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Rule Element</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getName <em>Name</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getType <em>Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getBelongsToSituation <em>Belongs To Situation</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElement()
 * @model
 * @generated
 */
public interface RuleElement extends EObject {
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
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElement_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getName <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Name</em>' attribute.
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Returns the value of the '<em><b>Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Type</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Type</em>' reference.
	 * @see #setType(RuleElementType)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElement_Type()
	 * @model required="true"
	 * @generated
	 */
	RuleElementType getType();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.RuleElement#getType <em>Type</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Type</em>' reference.
	 * @see #getType()
	 * @generated
	 */
	void setType(RuleElementType value);

	/**
	 * Returns the value of the '<em><b>Belongs To Situation</b></em>' reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.Situation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Belongs To Situation</em>' reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Belongs To Situation</em>' reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getRuleElement_BelongsToSituation()
	 * @model type="model2.Situation"
	 * @generated
	 */
	EList getBelongsToSituation();

} // RuleElement
