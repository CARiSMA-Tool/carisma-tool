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
 * A representation of the model object '<em><b>Container</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsAllocation <em>Contains Allocation</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsSituation <em>Contains Situation</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsBPMNElement <em>Contains BPMN Element</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociation <em>Contains Rule Element Association</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElement <em>Contains Rule Element</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElemntType <em>Contains Rule Elemnt Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsRuleElementAssociationType <em>Contains Rule Element Association Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.Container#getContainsModelType <em>Contains Model Type</em>}</li>
 * </ul>
 * </p>
 *
 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer()
 * @model
 * @generated
 */
public interface Container extends EObject {
	/**
	 * Returns the value of the '<em><b>Contains Allocation</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.Allocation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Allocation</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Allocation</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsAllocation()
	 * @model type="model2.Allocation" containment="true"
	 * @generated
	 */
	EList getContainsAllocation();

	/**
	 * Returns the value of the '<em><b>Contains Situation</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.Situation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Situation</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Situation</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsSituation()
	 * @model type="model2.Situation" containment="true"
	 * @generated
	 */
	EList getContainsSituation();

	/**
	 * Returns the value of the '<em><b>Contains BPMN Element</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.BPMNElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains BPMN Element</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains BPMN Element</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsBPMNElement()
	 * @model type="model2.BPMNElement" containment="true"
	 * @generated
	 */
	EList getContainsBPMNElement();

	/**
	 * Returns the value of the '<em><b>Contains Rule Element Association</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Rule Element Association</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Rule Element Association</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsRuleElementAssociation()
	 * @model type="model2.RuleElementAssosation" containment="true"
	 * @generated
	 */
	EList getContainsRuleElementAssociation();

	/**
	 * Returns the value of the '<em><b>Contains Rule Element</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.RuleElement}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Rule Element</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Rule Element</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsRuleElement()
	 * @model type="model2.RuleElement" containment="true"
	 * @generated
	 */
	EList getContainsRuleElement();

	/**
	 * Returns the value of the '<em><b>Contains Rule Elemnt Type</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.RuleElementType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Rule Elemnt Type</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Rule Elemnt Type</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsRuleElemntType()
	 * @model type="model2.RuleElementType" containment="true"
	 * @generated
	 */
	EList getContainsRuleElementType();

	/**
	 * Returns the value of the '<em><b>Contains Rule Element Association Type</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Rule Element Association Type</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Rule Element Association Type</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsRuleElementAssociationType()
	 * @model type="model2.RuleElementAssosiationType" containment="true"
	 * @generated
	 */
	EList getContainsRuleElementAssociationType();

	/**
	 * Returns the value of the '<em><b>Contains Model Type</b></em>' containment reference list.
	 * The list contents are of type {@link carisma.regulatory.ruleallocator.datamodel.ModelElementType}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Contains Model Type</em>' containment reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Contains Model Type</em>' containment reference list.
	 * @see carisma.regulatory.ruleallocator.datamodel.DatamodelPackage#getContainer_ContainsModelType()
	 * @model type="model2.ModelElementType" containment="true"
	 * @generated
	 */
	EList getContainsModelType();

} // Container
