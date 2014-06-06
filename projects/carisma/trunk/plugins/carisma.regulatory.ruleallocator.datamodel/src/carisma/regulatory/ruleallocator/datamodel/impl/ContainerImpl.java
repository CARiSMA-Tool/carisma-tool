/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.impl;

import java.util.Collection;


import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import carisma.regulatory.ruleallocator.datamodel.Allocation;
import carisma.regulatory.ruleallocator.datamodel.BPMNElement;
import carisma.regulatory.ruleallocator.datamodel.Container;
import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.ModelElementType;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;
import carisma.regulatory.ruleallocator.datamodel.Situation;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Container</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsAllocation <em>Contains Allocation</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsSituation <em>Contains Situation</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsBPMNElement <em>Contains BPMN Element</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsRuleElementAssociation <em>Contains Rule Element Association</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsRuleElement <em>Contains Rule Element</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsRuleElemntType <em>Contains Rule Elemnt Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsRuleElementAssociationType <em>Contains Rule Element Association Type</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.ContainerImpl#getContainsModelType <em>Contains Model Type</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class ContainerImpl extends EObjectImpl implements Container {
	/**
	 * The cached value of the '{@link #getContainsAllocation() <em>Contains Allocation</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsAllocation()
	 * @generated
	 * @ordered
	 */
	protected EList containsAllocation;

	/**
	 * The cached value of the '{@link #getContainsSituation() <em>Contains Situation</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsSituation()
	 * @generated
	 * @ordered
	 */
	protected EList containsSituation;

	/**
	 * The cached value of the '{@link #getContainsBPMNElement() <em>Contains BPMN Element</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsBPMNElement()
	 * @generated
	 * @ordered
	 */
	protected EList containsBPMNElement;

	/**
	 * The cached value of the '{@link #getContainsRuleElementAssociation() <em>Contains Rule Element Association</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsRuleElementAssociation()
	 * @generated
	 * @ordered
	 */
	protected EList containsRuleElementAssociation;

	/**
	 * The cached value of the '{@link #getContainsRuleElement() <em>Contains Rule Element</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsRuleElement()
	 * @generated
	 * @ordered
	 */
	protected EList containsRuleElement;

	/**
	 * The cached value of the '{@link #getContainsRuleElementType() <em>Contains Rule Elemnt Type</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsRuleElementType()
	 * @generated
	 * @ordered
	 */
	protected EList containsRuleElementType;

	/**
	 * The cached value of the '{@link #getContainsRuleElementAssociationType() <em>Contains Rule Element Association Type</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsRuleElementAssociationType()
	 * @generated
	 * @ordered
	 */
	protected EList containsRuleElementAssociationType;

	/**
	 * The cached value of the '{@link #getContainsModelType() <em>Contains Model Type</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getContainsModelType()
	 * @generated
	 * @ordered
	 */
	protected EList containsModelType;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected ContainerImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return DatamodelPackage.Literals.CONTAINER;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsAllocation() {
		if (containsAllocation == null) {
			containsAllocation = new EObjectContainmentEList(Allocation.class, this, DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION);
		}
		return containsAllocation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsSituation() {
		if (containsSituation == null) {
			containsSituation = new EObjectContainmentEList(Situation.class, this, DatamodelPackage.CONTAINER__CONTAINS_SITUATION);
		}
		return containsSituation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsBPMNElement() {
		if (containsBPMNElement == null) {
			containsBPMNElement = new EObjectContainmentEList(BPMNElement.class, this, DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT);
		}
		return containsBPMNElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsRuleElementAssociation() {
		if (containsRuleElementAssociation == null) {
			containsRuleElementAssociation = new EObjectContainmentEList(RuleElementAssociation.class, this, DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION);
		}
		return containsRuleElementAssociation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsRuleElement() {
		if (containsRuleElement == null) {
			containsRuleElement = new EObjectContainmentEList(RuleElement.class, this, DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT);
		}
		return containsRuleElement;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsRuleElementType() {
		if (containsRuleElementType == null) {
			containsRuleElementType = new EObjectContainmentEList(RuleElementType.class, this, DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE);
		}
		return containsRuleElementType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsRuleElementAssociationType() {
		if (containsRuleElementAssociationType == null) {
			containsRuleElementAssociationType = new EObjectContainmentEList(RuleElementAssociationType.class, this, DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE);
		}
		return containsRuleElementAssociationType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList getContainsModelType() {
		if (containsModelType == null) {
			containsModelType = new EObjectContainmentEList(ModelElementType.class, this, DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE);
		}
		return containsModelType;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION:
				return ((InternalEList)getContainsAllocation()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_SITUATION:
				return ((InternalEList)getContainsSituation()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT:
				return ((InternalEList)getContainsBPMNElement()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION:
				return ((InternalEList)getContainsRuleElementAssociation()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT:
				return ((InternalEList)getContainsRuleElement()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE:
				return ((InternalEList)getContainsRuleElementType()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE:
				return ((InternalEList)getContainsRuleElementAssociationType()).basicRemove(otherEnd, msgs);
			case DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE:
				return ((InternalEList)getContainsModelType()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION:
				return getContainsAllocation();
			case DatamodelPackage.CONTAINER__CONTAINS_SITUATION:
				return getContainsSituation();
			case DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT:
				return getContainsBPMNElement();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION:
				return getContainsRuleElementAssociation();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT:
				return getContainsRuleElement();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE:
				return getContainsRuleElementType();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE:
				return getContainsRuleElementAssociationType();
			case DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE:
				return getContainsModelType();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION:
				getContainsAllocation().clear();
				getContainsAllocation().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_SITUATION:
				getContainsSituation().clear();
				getContainsSituation().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT:
				getContainsBPMNElement().clear();
				getContainsBPMNElement().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION:
				getContainsRuleElementAssociation().clear();
				getContainsRuleElementAssociation().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT:
				getContainsRuleElement().clear();
				getContainsRuleElement().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE:
				getContainsRuleElementType().clear();
				getContainsRuleElementType().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE:
				getContainsRuleElementAssociationType().clear();
				getContainsRuleElementAssociationType().addAll((Collection)newValue);
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE:
				getContainsModelType().clear();
				getContainsModelType().addAll((Collection)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void eUnset(int featureID) {
		switch (featureID) {
			case DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION:
				getContainsAllocation().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_SITUATION:
				getContainsSituation().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT:
				getContainsBPMNElement().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION:
				getContainsRuleElementAssociation().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT:
				getContainsRuleElement().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE:
				getContainsRuleElementType().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE:
				getContainsRuleElementAssociationType().clear();
				return;
			case DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE:
				getContainsModelType().clear();
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DatamodelPackage.CONTAINER__CONTAINS_ALLOCATION:
				return containsAllocation != null && !containsAllocation.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_SITUATION:
				return containsSituation != null && !containsSituation.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_BPMN_ELEMENT:
				return containsBPMNElement != null && !containsBPMNElement.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION:
				return containsRuleElementAssociation != null && !containsRuleElementAssociation.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT:
				return containsRuleElement != null && !containsRuleElement.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMNT_TYPE:
				return containsRuleElementType != null && !containsRuleElementType.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_RULE_ELEMENT_ASSOCIATION_TYPE:
				return containsRuleElementAssociationType != null && !containsRuleElementAssociationType.isEmpty();
			case DatamodelPackage.CONTAINER__CONTAINS_MODEL_TYPE:
				return containsModelType != null && !containsModelType.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //ContainerImpl
