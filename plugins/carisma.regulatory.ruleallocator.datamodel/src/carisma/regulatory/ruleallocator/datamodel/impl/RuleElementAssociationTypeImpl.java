/**
 * <copyright>
 * </copyright>
 *
 * $Id$
 */
package carisma.regulatory.ruleallocator.datamodel.impl;


import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.impl.EObjectImpl;

import carisma.regulatory.ruleallocator.datamodel.DatamodelPackage;
import carisma.regulatory.ruleallocator.datamodel.RuleElementAssociationType;
import carisma.regulatory.ruleallocator.datamodel.RuleElementType;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Rule Element Assosiation Type</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl#getSrc <em>Src</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl#getTarget <em>Target</em>}</li>
 *   <li>{@link carisma.regulatory.ruleallocator.datamodel.impl.RuleElementAssociationTypeImpl#getName <em>Name</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class RuleElementAssociationTypeImpl extends EObjectImpl implements RuleElementAssociationType {
	/**
	 * The cached value of the '{@link #getSrc() <em>Src</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getSrc()
	 * @generated
	 * @ordered
	 */
	protected RuleElementType src;

	/**
	 * The cached value of the '{@link #getTarget() <em>Target</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTarget()
	 * @generated
	 * @ordered
	 */
	protected RuleElementType target;

	/**
	 * The default value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected static final String NAME_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getName() <em>Name</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getName()
	 * @generated
	 * @ordered
	 */
	protected String name = NAME_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected RuleElementAssociationTypeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected EClass eStaticClass() {
		return DatamodelPackage.Literals.RULE_ELEMENT_ASSOSIATION_TYPE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementType getSrc() {
		if (src != null && src.eIsProxy()) {
			InternalEObject oldSrc = (InternalEObject)src;
			src = (RuleElementType)eResolveProxy(oldSrc);
			if (src != oldSrc) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC, oldSrc, src));
			}
		}
		return src;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementType basicGetSrc() {
		return src;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setSrc(RuleElementType newSrc) {
		RuleElementType oldSrc = src;
		src = newSrc;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC, oldSrc, src));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementType getTarget() {
		if (target != null && target.eIsProxy()) {
			InternalEObject oldTarget = (InternalEObject)target;
			target = (RuleElementType)eResolveProxy(oldTarget);
			if (target != oldTarget) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET, oldTarget, target));
			}
		}
		return target;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public RuleElementType basicGetTarget() {
		return target;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTarget(RuleElementType newTarget) {
		RuleElementType oldTarget = target;
		target = newTarget;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET, oldTarget, target));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String getName() {
		return name;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setName(String newName) {
		String oldName = name;
		name = newName;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__NAME, oldName, name));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC:
				if (resolve) return getSrc();
				return basicGetSrc();
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET:
				if (resolve) return getTarget();
				return basicGetTarget();
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__NAME:
				return getName();
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC:
				setSrc((RuleElementType)newValue);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET:
				setTarget((RuleElementType)newValue);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__NAME:
				setName((String)newValue);
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC:
				setSrc((RuleElementType)null);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET:
				setTarget((RuleElementType)null);
				return;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__NAME:
				setName(NAME_EDEFAULT);
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
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__SRC:
				return src != null;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__TARGET:
				return target != null;
			case DatamodelPackage.RULE_ELEMENT_ASSOSIATION_TYPE__NAME:
				return NAME_EDEFAULT == null ? name != null : !NAME_EDEFAULT.equals(name);
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (name: ");
		result.append(name);
		result.append(')');
		return result.toString();
	}

} //RuleElementAssosiationTypeImpl
