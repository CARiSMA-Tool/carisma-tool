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
 * A representation of the model object '<em><b>Allocation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getRuleElement <em>Rule Element</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getBpmnElement <em>Bpmn Element</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getAllocation()
 * @model
 * @generated
 */
public interface Allocation extends EObject {
	/**
	 * Returns the value of the '<em><b>Rule Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Rule Element</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Rule Element</em>' reference.
	 * @see #setRuleElement(RuleElement)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getAllocation_RuleElement()
	 * @model required="true"
	 * @generated
	 */
	RuleElement getRuleElement();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getRuleElement <em>Rule Element</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Rule Element</em>' reference.
	 * @see #getRuleElement()
	 * @generated
	 */
	void setRuleElement(RuleElement value);

	/**
	 * Returns the value of the '<em><b>Bpmn Element</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Bpmn Element</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Bpmn Element</em>' reference.
	 * @see #setBpmnElement(BPMNElement)
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getAllocation_BpmnElement()
	 * @model required="true"
	 * @generated
	 */
	BPMNElement getBpmnElement();

	/**
	 * Sets the value of the '{@link carisma.regulatory.ruleallocator.datamodel.Allocation#getBpmnElement <em>Bpmn Element</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Bpmn Element</em>' reference.
	 * @see #getBpmnElement()
	 * @generated
	 */
	void setBpmnElement(BPMNElement value);

} // Allocation
