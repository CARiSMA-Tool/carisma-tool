/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.impl;


import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.RuleElement;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociation;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Rule Element Assosation</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl#getSrc1 <em>Src1</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl#getTarget1 <em>Target1</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationImpl#getType1 <em>Type1</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RuleElementAssociationImpl extends EObjectImpl implements RuleElementAssociation {
	/**
	 * The cached value of the '{@link #getSrc1() <em>Src1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSrc1()
	 * @generated
	 * @ordered
	 */
	protected RuleElement src1;

	/**
	 * The cached value of the '{@link #getTarget1() <em>Target1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTarget1()
	 * @generated
	 * @ordered
	 */
	protected RuleElement target1;

	/**
	 * The cached value of the '{@link #getType1() <em>Type1</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getType1()
	 * @generated
	 * @ordered
	 */
	protected RuleElementAssociationType type1;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RuleElementAssociationImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return DatamodelPackage.Literals.RULE_ELEMENT_ASSOSATION;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElement getSrc1() {
		if (src1 != null && src1.eIsProxy()) {
			InternalEObject oldSrc1 = (InternalEObject)src1;
			src1 = (RuleElement)eResolveProxy(oldSrc1);
			if (src1 != oldSrc1) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1, oldSrc1, src1));
			}
		}
		return src1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElement basicGetSrc1() {
		return src1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSrc1(RuleElement newSrc1) {
		RuleElement oldSrc1 = src1;
		src1 = newSrc1;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1, oldSrc1, src1));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElement getTarget1() {
		if (target1 != null && target1.eIsProxy()) {
			InternalEObject oldTarget1 = (InternalEObject)target1;
			target1 = (RuleElement)eResolveProxy(oldTarget1);
			if (target1 != oldTarget1) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1, oldTarget1, target1));
			}
		}
		return target1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElement basicGetTarget1() {
		return target1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTarget1(RuleElement newTarget1) {
		RuleElement oldTarget1 = target1;
		target1 = newTarget1;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1, oldTarget1, target1));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementAssociationType getType1() {
		if (type1 != null && type1.eIsProxy()) {
			InternalEObject oldType1 = (InternalEObject)type1;
			type1 = (RuleElementAssociationType)eResolveProxy(oldType1);
			if (type1 != oldType1) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1, oldType1, type1));
			}
		}
		return type1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementAssociationType basicGetType1() {
		return type1;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setType1(RuleElementAssociationType newType1) {
		RuleElementAssociationType oldType1 = type1;
		type1 = newType1;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1, oldType1, type1));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1:
				if (resolve) return getSrc1();
				return basicGetSrc1();
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1:
				if (resolve) return getTarget1();
				return basicGetTarget1();
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1:
				if (resolve) return getType1();
				return basicGetType1();
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1:
				setSrc1((RuleElement)newValue);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1:
				setTarget1((RuleElement)newValue);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1:
				setType1((RuleElementAssociationType)newValue);
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1:
				setSrc1((RuleElement)null);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1:
				setTarget1((RuleElement)null);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1:
				setType1((RuleElementAssociationType)null);
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__SRC1:
				return src1 != null;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TARGET1:
				return target1 != null;
			case DatamodelPackage.RULE_ELEMENT_ASSOSATION__TYPE1:
				return type1 != null;
		}
		return super.eIsSet(featureID);
	}

} //RuleElementAssosationImpl
